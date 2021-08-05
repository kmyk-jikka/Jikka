{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.TrivialLetElimination
-- Description : removes let-exprs whose variables are referenced at most only once. / その変数が高々 1 回しか参照されないような let 式を消去します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.TrivialLetElimination
  ( run,
    run',
  )
where

import Data.Functor
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint

plus :: Maybe Bool -> Maybe Bool -> Maybe Bool
plus (Just _) (Just _) = Just False
plus (Just p) Nothing = Just p
plus Nothing (Just p) = Just p
plus Nothing Nothing = Nothing

isEliminatable :: VarName -> Expr -> Maybe Bool
isEliminatable x = \case
  Var y -> if x == y then Just True else Nothing
  Lit _ -> Nothing
  App f e -> isEliminatable x f `plus` isEliminatable x e
  Lam y _ e -> if x == y then Nothing else isEliminatable x e $> False -- moving an expr into a lambda may increase the time complexity
  Let y _ e1 e2 -> isEliminatable x e1 `plus` (if x == y then Nothing else isEliminatable x e2)
  Assert e1 e2 -> isEliminatable x e1 `plus` isEliminatable x e2

isEliminatableToplevelExpr :: VarName -> ToplevelExpr -> Maybe Bool
isEliminatableToplevelExpr x = \case
  ResultExpr e -> isEliminatable x e
  ToplevelLet y _ e cont -> isEliminatable x e `plus` (if x == y then Nothing else isEliminatableToplevelExpr x cont)
  ToplevelLetRec f args _ body cont -> if x == f then Nothing else isEliminatableToplevelExpr x cont `plus` (if x `elem` map fst args then Nothing else isEliminatable x body)
  ToplevelAssert e cont -> isEliminatable x e `plus` isEliminatableToplevelExpr x cont

runExpr :: M.Map VarName Expr -> Expr -> Expr
runExpr env = \case
  Var x -> fromMaybe (Var x) (M.lookup x env)
  Lit lit -> Lit lit
  App f e -> App (runExpr env f) (runExpr env e)
  Lam x t body -> Lam x t (runExpr env body)
  Let x t e1 e2 ->
    let e1' = runExpr env e1
     in if isEliminatable x e2 /= Just False
          then runExpr (M.insert x e1' env) e2
          else Let x t e1' (runExpr env e2)
  Assert e1 e2 -> Assert (runExpr env e1) (runExpr env e2)

runToplevelExpr :: M.Map VarName Expr -> ToplevelExpr -> ToplevelExpr
runToplevelExpr env = \case
  ResultExpr e -> ResultExpr (runExpr env e)
  ToplevelLet x t e cont ->
    let e' = runExpr env e
     in if isEliminatableToplevelExpr x cont /= Just False
          then runToplevelExpr (M.insert x e' env) cont
          else ToplevelLet x t e' (runToplevelExpr env cont)
  ToplevelLetRec f args ret body cont ->
    ToplevelLetRec f args ret (runExpr env body) (runToplevelExpr env cont)
  ToplevelAssert e cont ->
    ToplevelAssert (runExpr env e) (runToplevelExpr env cont)

run' :: Program -> Program
run' = runToplevelExpr M.empty

-- | `run` remove let-exprs whose assigned variables are used only at most once.
-- This assumes that the program is alpha-converted.
--
-- For example, this converts the following:
--
-- > let f = fun y -> y
-- > in let x = 1
-- > in f(x + x)
--
-- to:
--
-- > let x = 1
-- > in (fun y -> y) (x + x)
--
-- NOTE: this doesn't constant folding.
run :: MonadError Error m => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.ConstantPropagation" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- return $ run' prog
  postcondition $ do
    ensureWellTyped prog
  return prog
