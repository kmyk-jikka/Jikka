{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.PropagateMod
-- Description : propagates modulo operations, and replaces integer functions with corresponding functions with modulo. / 剰余演算を伝播させ、整数の関数を対応する modulo 付きの関数で置き換えます。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.PropagateMod
  ( run,
  )
where

import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Format (formatType)
import Jikka.Core.Language.Beta
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.ModuloExpr
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.TypeCheck
import Jikka.Core.Language.Util

isModulo' :: Expr -> Expr -> Bool
isModulo' e m = e `isModulo` Modulo m

putFloorMod :: MonadAlpha m => Modulo -> Expr -> m (Maybe Expr)
putFloorMod (Modulo m) =
  runMaybeT . \case
    Negate' e -> return $ ModNegate' e m
    Plus' e1 e2 -> return $ ModPlus' e1 e2 m
    Minus' e1 e2 -> return $ ModMinus' e1 e2 m
    Mult' e1 e2 -> return $ ModMult' e1 e2 m
    JustDiv' e1 e2 -> return $ ModMult' e1 (ModInv' e2 m) m
    Pow' e1 e2 -> return $ ModPow' e1 e2 m
    MatAp' h w e1 e2 -> return $ ModMatAp' h w e1 e2 m
    MatAdd' h w e1 e2 -> return $ ModMatAdd' h w e1 e2 m
    MatMul' h n w e1 e2 -> return $ ModMatMul' h n w e1 e2 m
    MatPow' n e1 e2 -> return $ ModMatPow' n e1 e2 m
    Sum' e -> return $ ModSum' e m
    Product' e -> return $ ModProduct' e m
    LitInt' n -> case m of
      LitInt' m -> return $ LitInt' (n `mod` m)
      _ -> MaybeT $ return Nothing
    Proj' ts i e | isVectorTy' ts -> return $ Proj' ts i (VecFloorMod' (genericLength ts) e m)
    Proj' ts i e
      | isMatrixTy' ts ->
        let (h, w) = fromJust (sizeOfMatrixTy (TupleTy ts))
         in return $ Proj' ts i (MatFloorMod' (toInteger h) (toInteger w) e m)
    Map' t1 t2 f xs -> do
      f <- MaybeT $ putFloorMod (Modulo m) f
      return $ Map' t1 t2 f xs
    Foldl' t1 t2 f init xs -> do
      f <- MaybeT $ putFloorMod (Modulo m) f
      return $ Foldl' t1 t2 f init xs
    Lam x t body -> do
      -- TODO: rename only if required
      y <- lift $ genVarName x
      body <- lift $ substitute x (Var y) body
      body <- MaybeT $ putFloorMod (Modulo m) body
      return $ Lam y t body
    e@(App _ _) -> case curryApp e of
      (f@(Lam _ _ _), args) -> do
        f <- MaybeT $ putFloorMod (Modulo m) f
        return $ uncurryApp f args
      (Tuple' ts, es) | isVectorTy' ts -> do
        es' <- lift $ mapM (putFloorMod (Modulo m)) es
        if all isNothing es'
          then MaybeT $ return Nothing
          else return $ uncurryApp (Tuple' ts) (zipWith fromMaybe es es')
      (Tuple' ts, es) | isMatrixTy (TupleTy ts) -> do
        es' <- lift $ mapM (putFloorMod (Modulo m)) es
        if all isNothing es'
          then MaybeT $ return Nothing
          else return $ uncurryApp (Tuple' ts) (zipWith fromMaybe es es')
      _ -> MaybeT $ return Nothing
    _ -> MaybeT $ return Nothing

putFloorModGeneric :: MonadAlpha m => (Expr -> Modulo -> m Expr) -> Modulo -> Expr -> m Expr
putFloorModGeneric fallback m e =
  if e `isModulo` m
    then return e
    else do
      e' <- putFloorMod m e
      case e' of
        Just e' -> return e'
        Nothing -> fallback e m

putMapFloorMod :: MonadAlpha m => Modulo -> Expr -> m Expr
putMapFloorMod = putFloorModGeneric fallback
  where
    fallback e (Modulo m) = do
      x <- genVarName'
      return $ Map' IntTy IntTy (Lam x IntTy (FloorMod' (Var x) m)) e

putVecFloorMod :: (MonadError Error m, MonadAlpha m) => [(VarName, Type)] -> Modulo -> Expr -> m Expr
putVecFloorMod env = putFloorModGeneric fallback
  where
    fallback e (Modulo m) = do
      t <- typecheckExpr env e
      case t of
        TupleTy ts -> return $ VecFloorMod' (genericLength ts) e m
        _ -> throwInternalError $ "not a vector: " ++ formatType t

putMatFloorMod :: (MonadError Error m, MonadAlpha m) => [(VarName, Type)] -> Modulo -> Expr -> m Expr
putMatFloorMod env = putFloorModGeneric fallback
  where
    fallback e (Modulo m) = do
      t <- typecheckExpr env e
      case t of
        TupleTy ts@(TupleTy ts' : _) -> return $ MatFloorMod' (genericLength ts) (genericLength ts') e m
        _ -> throwInternalError $ "not a matrix: " ++ formatType t

rule :: (MonadAlpha m, MonadError Error m) => RewriteRule m
rule =
  let go0 :: Expr -> Maybe Expr
      go0 e = do
        e' <- formatBottomModuloExpr <$> parseModuloExpr e
        guard $ e' /= e
        return e'
      go1 m f (t1, e1) = Just <$> (f <$> t1 (Modulo m) e1 <*> pure m)
      go2 m f (t1, e1) (t2, e2) = Just <$> (f <$> t1 (Modulo m) e1 <*> t2 (Modulo m) e2 <*> pure m)
   in makeRewriteRule "Jikka.Core.Convert.PropagateMod" $ \env -> \case
        e@(ModNegate' _ _) -> return $ go0 e
        e@(ModPlus' _ _ _) -> return $ go0 e
        e@(ModMinus' _ _ _) -> return $ go0 e
        e@(ModMult' _ _ _) -> return $ go0 e
        e@(ModInv' _ _) -> return $ go0 e
        e@(ModPow' _ _ _) -> return $ go0 e
        ModMatAp' h w e1 e2 m | not (e1 `isModulo'` m) || not (e2 `isModulo'` m) -> go2 m (ModMatAp' h w) (putMatFloorMod (typeEnv env), e1) (putVecFloorMod (typeEnv env), e2)
        ModMatAdd' h w e1 e2 m | not (e1 `isModulo'` m) || not (e2 `isModulo'` m) -> go2 m (ModMatAdd' h w) (putMatFloorMod (typeEnv env), e1) (putMatFloorMod (typeEnv env), e2)
        ModMatMul' h n w e1 e2 m | not (e1 `isModulo'` m) || not (e2 `isModulo'` m) -> go2 m (ModMatMul' h n w) (putMatFloorMod (typeEnv env), e1) (putMatFloorMod (typeEnv env), e2)
        ModMatPow' n e1 e2 m | not (e1 `isModulo'` m) -> go2 m (ModMatPow' n) (putMatFloorMod (typeEnv env), e1) (\_ e -> return e, e2)
        ModSum' e m | not (e `isModulo'` m) -> go1 m ModSum' (putMapFloorMod, e)
        ModProduct' e m | not (e `isModulo'` m) -> go1 m ModProduct' (putMapFloorMod, e)
        FloorMod' e m ->
          if e `isModulo'` m
            then return $ Just e
            else putFloorMod (Modulo m) e
        VecFloorMod' _ e m ->
          if e `isModulo'` m
            then return $ Just e
            else putFloorMod (Modulo m) e
        MatFloorMod' _ _ e m ->
          if e `isModulo'` m
            then return $ Just e
            else putFloorMod (Modulo m) e
        _ -> return Nothing

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` propagates `FloorMod` to leaves of exprs.
-- For example, this converts the following:
--
-- > mod ((fun x -> x * x + x) y) 1000000007
--
-- to:
--
-- > (fun x -> mod (mod (x * x) 1000000007 + x) 1000000007) y
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.PropagateMod" $ do
  precondition $ do
    lint prog
  prog <- runProgram prog
  postcondition $ do
    lint prog
  return prog
