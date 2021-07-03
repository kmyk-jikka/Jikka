{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Convert.TrivialLetElimination
  ( run,
    run',
  )
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Util

type Env = M.Map VarName Expr

runExpr :: Env -> Expr -> Expr
runExpr env = \case
  Var x -> fromMaybe (Var x) (M.lookup x env)
  Lit lit -> Lit lit
  App f e -> App (runExpr env f) (runExpr env e)
  Lam x t body -> Lam x t (runExpr env body)
  Let x t e1 e2 ->
    let e1' = runExpr env e1
     in if countOccurrences x e2 <= 1
          then runExpr (M.insert x e1' env) e2
          else Let x t e1' (runExpr env e2)

runToplevelExpr :: Env -> ToplevelExpr -> ToplevelExpr
runToplevelExpr env = \case
  ResultExpr e -> ResultExpr (runExpr env e)
  ToplevelLet x t e cont ->
    let e' = runExpr env e
     in if countOccurrencesToplevelExpr x cont <= 1
          then runToplevelExpr (M.insert x e' env) cont
          else ToplevelLet x t e' (runToplevelExpr env cont)
  ToplevelLetRec f args ret body cont ->
    ToplevelLetRec f args ret (runExpr env body) (runToplevelExpr env cont)

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
