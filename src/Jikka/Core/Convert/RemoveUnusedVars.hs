{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.RemoveUnusedVars
-- Description : remove unused variables from exprs.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.RemoveUnusedVars` remove unused variables from exprs.
module Jikka.Core.Convert.RemoveUnusedVars
  ( run,
    run',
  )
where

import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Util
import Jikka.Core.Language.Vars (isUnusedVar)

runLet :: VarName -> Type -> Expr -> Expr -> Expr
runLet x t e1 e2
  | isUnusedVar x e2 = e2
  | otherwise = Let x t e1 e2

runExpr :: Expr -> Expr
runExpr = \case
  Var x -> Var x
  Lit lit -> Lit lit
  App f e -> App (runExpr f) (runExpr e)
  Lam x t e -> Lam x t (runExpr e)
  Let x t e1 e2 -> runLet x t (runExpr e1) (runExpr e2)

runToplevelExpr :: ToplevelExpr -> ToplevelExpr
runToplevelExpr = \case
  ResultExpr e -> ResultExpr $ runExpr e
  ToplevelLet x t e cont -> ToplevelLet x t (runExpr e) (runToplevelExpr cont)
  ToplevelLetRec f args ret body cont ->
    let body' = runExpr body
        cont' = runToplevelExpr cont
     in if isUnusedVar f body'
          then ToplevelLet f (curryFunTy (map snd args) ret) (curryLam args body') cont'
          else ToplevelLetRec f args ret body' cont'

run' :: Program -> Program
run' = runToplevelExpr

-- | `run` removes unused variables in given programs.
--
-- This also removes variables for recursion, i.e. "rec" flags.
-- `ToplevelLetRec` may becomes `ToplevelLet`.
--
-- For example, this converts
--
-- > let rec solve x =
-- >     let y = 0
-- >     in x
-- > in solve
--
-- to
--
-- > let solve x =
-- >     x
-- > in solve
run :: MonadError Error m => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.RemoveUnusedVars" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- return $ run' prog
  postcondition $ do
    ensureWellTyped prog
  return prog
