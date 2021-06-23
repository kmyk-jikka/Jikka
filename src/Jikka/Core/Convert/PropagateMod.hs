{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Convert.PropagateMod
  ( run,
    run',
  )
where

import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint

runFloorMod :: Expr -> Expr -> Expr
runFloorMod e m = case runExpr e of
  FloorMod' e m' ->
    if m == m'
      then runFloorMod e m
      else runFloorMod e (Lcm' m m')
  Negate' e -> FloorMod' (Negate' (runFloorMod e m)) m
  Plus' e1 e2 -> FloorMod' (Plus' (runFloorMod e1 m) (runFloorMod e2 m)) m
  Minus' e1 e2 -> FloorMod' (Minus' (runFloorMod e1 m) (runFloorMod e2 m)) m
  Mult' e1 e2 -> FloorMod' (Mult' (runFloorMod e1 m) (runFloorMod e2 m)) m
  Pow' e1 e2 -> ModPow' (runFloorMod e1 m) e2 m
  ModInv' e m' ->
    if m == m'
      then ModInv' (runFloorMod e m) m
      else FloorMod' (ModInv' (runFloorMod e m') m') m
  ModPow' e1 e2 m' ->
    if m == m'
      then ModPow' (runFloorMod e1 m) e2 m
      else FloorMod' (ModPow' (runFloorMod e1 m') e2 m') m
  App (Lam args body) es -> App (Lam args (runFloorMod body m)) (map runExpr es)
  e -> FloorMod' e m

runExpr :: Expr -> Expr
runExpr = \case
  FloorMod' e m -> runFloorMod e m
  Var x -> Var x
  Lit lit -> Lit lit
  App f args -> App (runExpr f) (map runExpr args)
  Lam args body -> Lam args (runExpr body)
  Let x t e1 e2 -> Let x t (runExpr e1) (runExpr e2)

runToplevelExpr :: ToplevelExpr -> ToplevelExpr
runToplevelExpr = \case
  ResultExpr e -> ResultExpr (runExpr e)
  ToplevelLet x t e cont -> ToplevelLet x t (runExpr e) (runToplevelExpr cont)
  ToplevelLetRec f args ret body cont -> ToplevelLetRec f args ret (runExpr body) (runToplevelExpr cont)

run' :: Program -> Program
run' = runToplevelExpr

-- | `run` propagates `FloorMod` to leaves of exprs.
-- For example, this converts the following:
--
-- > mod ((fun x -> x * x + x) y) 1000000007
--
-- to:
--
-- > (fun x -> mod (mod (x * x) 1000000007 + x) 1000000007) y
run :: MonadError Error m => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.ConstantPropagation" $ do
  prog <- return $ run' prog
  ensureWellTyped prog
  return prog
