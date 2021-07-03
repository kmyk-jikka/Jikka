{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Convert.ConstantPropagation
  ( run,
    run',

    -- * internal functions
    isSmallExpr,
  )
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint

type Env = M.Map VarName Expr

-- | `isSmallExpr` checks whether given exprs are suitable to propagate.
isSmallExpr :: Expr -> Bool
isSmallExpr = \case
  Var _ -> True
  Lit _ -> True
  App f e -> isSmallExpr f && isSmallExpr e
  Lam _ _ _ -> False
  Let _ _ _ _ -> False

runExpr :: Env -> Expr -> Expr
runExpr env = \case
  Var x -> fromMaybe (Var x) (M.lookup x env)
  Lit lit -> Lit lit
  App f e -> App (runExpr env f) (runExpr env e)
  Lam x t body -> Lam x t (runExpr env body)
  Let x t e1 e2 ->
    let e1' = runExpr env e1
     in if isSmallExpr e1'
          then runExpr (M.insert x e1' env) e2
          else Let x t e1' (runExpr env e2)

runToplevelExpr :: Env -> ToplevelExpr -> ToplevelExpr
runToplevelExpr env = \case
  ResultExpr e -> ResultExpr (runExpr env e)
  ToplevelLet x t e cont ->
    let e' = runExpr env e
     in if isSmallExpr e'
          then runToplevelExpr (M.insert x e' env) cont
          else ToplevelLet x t e' (runToplevelExpr env cont)
  ToplevelLetRec f args ret body cont ->
    ToplevelLetRec f args ret (runExpr env body) (runToplevelExpr env cont)

run' :: Program -> Program
run' = runToplevelExpr M.empty

-- | `run` does constant propagation.
-- This assumes that the program is alpha-converted.
--
-- For example, this converts the following:
--
-- > let x = 1
-- > in let f = fun y -> y
-- > in x + x + f(x)
--
-- to:
--
-- > let f = fun y -> y
-- > in 1 + 1 + f(1)
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
