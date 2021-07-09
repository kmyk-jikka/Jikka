{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.ConstantPropagation
-- Description : propagates something constants, for exprs which are computable with constant time. / 定数時間で計算できるような式についての、ある種の定数伝播をします。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.ConstantPropagation
  ( run,
    run',

    -- * internal functions
    isConstantTimeExpr,
  )
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Util

type Env = M.Map VarName Expr

isConstantTimeBuiltin :: Builtin -> Bool
isConstantTimeBuiltin = \case
  -- arithmetical functions
  Negate -> True
  Plus -> True
  Minus -> True
  Mult -> True
  FloorDiv -> True
  FloorMod -> True
  CeilDiv -> True
  CeilMod -> True
  Pow -> True
  -- advanced arithmetical functions
  Abs -> True
  Gcd -> True
  Lcm -> True
  Min2 _ -> True
  Max2 _ -> True
  Iterate _ -> False
  -- logical functions
  Not -> True
  And -> True
  Or -> True
  Implies -> True
  If _ -> True
  -- bitwise functions
  BitNot -> True
  BitAnd -> True
  BitOr -> True
  BitXor -> True
  BitLeftShift -> True
  BitRightShift -> True
  -- matrix functions
  MatAp _ _ -> True
  MatZero _ -> True
  MatOne _ -> True
  MatAdd _ _ -> True
  MatMul _ _ _ -> True
  MatPow _ -> True
  VecFloorMod _ -> True
  MatFloorMod _ _ -> True
  -- modular functions
  ModNegate -> True
  ModPlus -> True
  ModMinus -> True
  ModMult -> True
  ModInv -> True
  ModPow -> True
  ModMatAp _ _ -> True
  ModMatAdd _ _ -> True
  ModMatMul _ _ _ -> True
  ModMatPow _ -> True
  -- list functions
  Cons _ -> False
  Foldl _ _ -> False
  Scanl _ _ -> False
  Len _ -> True
  Map _ _ -> False
  Filter _ -> False
  At _ -> True
  SetAt _ -> False
  Elem _ -> False
  Sum -> False
  Product -> False
  ModSum -> False
  ModProduct -> False
  Min1 _ -> False
  Max1 _ -> False
  ArgMin _ -> False
  ArgMax _ -> False
  All -> False
  Any -> False
  Sorted _ -> False
  Reversed _ -> False
  Range1 -> False
  Range2 -> False
  Range3 -> False
  -- tuple functions
  Tuple _ -> True
  Proj _ _ -> True
  -- comparison
  LessThan _ -> True
  LessEqual _ -> True
  GreaterThan _ -> True
  GreaterEqual _ -> True
  Equal _ -> True
  NotEqual _ -> True
  -- combinational functions
  Fact -> True
  Choose -> True
  Permute -> True
  MultiChoose -> True

-- | `isConstantTimeExpr` checks whether given exprs are suitable to propagate.
isConstantTimeExpr :: Expr -> Bool
isConstantTimeExpr = \case
  Var _ -> True
  Lit _ -> True
  e@(App _ _) -> case curryApp e of
    (Lit (LitBuiltin f), args) -> isConstantTimeBuiltin f && all isConstantTimeExpr args
    _ -> False
  Lam _ _ _ -> True
  Let _ _ e1 e2 -> isConstantTimeExpr e1 && isConstantTimeExpr e2

runExpr :: Env -> Expr -> Expr
runExpr env = \case
  Var x -> fromMaybe (Var x) (M.lookup x env)
  Lit lit -> Lit lit
  App f e -> App (runExpr env f) (runExpr env e)
  Lam x t body -> Lam x t (runExpr env body)
  Let x t e1 e2 ->
    let e1' = runExpr env e1
     in if isConstantTimeExpr e1'
          then runExpr (M.insert x e1' env) e2
          else Let x t e1' (runExpr env e2)

runToplevelExpr :: Env -> ToplevelExpr -> ToplevelExpr
runToplevelExpr env = \case
  ResultExpr e -> ResultExpr (runExpr env e)
  ToplevelLet x t e cont ->
    let e' = runExpr env e
     in if isConstantTimeExpr e'
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
