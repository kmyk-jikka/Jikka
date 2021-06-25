{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Convert.PropagateMod
  ( run,
  )
where

import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.TypeCheck
import Jikka.Core.Language.Util

runFloorMod :: MonadError Error m => [(VarName, Type)] -> Expr -> Expr -> m Expr
runFloorMod env e m = go e
  where
    go :: MonadError Error m => Expr -> m Expr
    go = \case
      Negate' e -> FloorMod' <$> (Negate' <$> go e) <*> pure m
      Plus' e1 e2 -> FloorMod' <$> (Plus' <$> go e1 <*> go e2) <*> pure m
      Minus' e1 e2 -> FloorMod' <$> (Minus' <$> go e1 <*> go e2) <*> pure m
      Mult' e1 e2 -> FloorMod' <$> (Mult' <$> go e1 <*> go e2) <*> pure m
      Pow' e1 e2 -> ModPow' <$> go e1 <*> pure e2 <*> pure m
      ModInv' e m' | m == m' -> ModInv' <$> go e <*> pure m
      ModPow' e1 e2 m' | m == m' -> ModPow' <$> go e1 <*> pure e2 <*> pure m
      MatAp' h w e1 e2 -> ModMatAp' h w <$> go e1 <*> go e2 <*> pure m
      MatAdd' h w e1 e2 -> ModMatAdd' h w <$> go e1 <*> go e2 <*> pure m
      MatMul' h n w e1 e2 -> ModMatMul' h n w <$> go e1 <*> go e2 <*> pure m
      MatPow' n e1 e2 -> ModMatPow' n <$> go e1 <*> pure e2 <*> pure m
      Proj' ts i e@MatAp' {} -> Proj' ts i <$> go e
      Proj' ts i e@ModMatAp' {} -> Proj' ts i <$> go e
      ModMatAp' h w e1 e2 m' | m == m' -> ModMatAp' h w <$> go e1 <*> go e2 <*> pure m
      ModMatAdd' h w e1 e2 m' | m == m' -> ModMatAdd' h w <$> go e1 <*> go e2 <*> pure m
      ModMatMul' h n w e1 e2 m' | m == m' -> ModMatMul' h n w <$> go e1 <*> go e2 <*> pure m
      ModMatPow' n e1 e2 m' | m == m' -> ModMatPow' n <$> go e1 <*> pure e2 <*> pure m
      App (Lam args body) es -> App <$> (Lam args <$> runFloorMod (reverse args ++ env) body m) <*> pure es
      Tuple' ts es -> Tuple' ts <$> mapM go es
      FloorMod' e m' ->
        if m == m'
          then go e
          else runFloorMod env e (Lcm' m m')
      e -> do
        t <- typecheckExpr env e
        return $ case t of
          IntTy -> FloorMod' e m
          _ -> e

runExpr :: MonadError Error m => [(VarName, Type)] -> Expr -> m Expr
runExpr env = \case
  FloorMod' e m -> runFloorMod env e m
  e -> return e

runProgram :: MonadError Error m => Program -> m Program
runProgram = mapExprProgramM runExpr

-- | `run` propagates `FloorMod` to leaves of exprs.
-- For example, this converts the following:
--
-- > mod ((fun x -> x * x + x) y) 1000000007
--
-- to:
--
-- > (fun x -> mod (mod (x * x) 1000000007 + x) 1000000007) y
run :: MonadError Error m => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.PropagateMod" $ do
  prog <- runProgram prog
  ensureWellTyped prog
  return prog
