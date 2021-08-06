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

import Data.List
import Data.Maybe
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Format (formatType)
import Jikka.Core.Language.Beta
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.TypeCheck
import Jikka.Core.Language.Util

-- | `Mod` is a newtype to avoid mistakes that swapping left and right of mod-op.
newtype Mod = Mod Expr

isModulo' :: Expr -> Mod -> Bool
isModulo' e (Mod m) = case e of
  FloorMod' _ m' -> m' == m
  ModNegate' _ m' -> m' == m
  ModPlus' _ _ m' -> m' == m
  ModMinus' _ _ m' -> m' == m
  ModMult' _ _ m' -> m' == m
  ModInv' _ m' -> m' == m
  ModPow' _ _ m' -> m' == m
  VecFloorMod' _ _ m' -> m' == m
  MatFloorMod' _ _ _ m' -> m' == m
  ModMatAp' _ _ _ _ m' -> m' == m
  ModMatAdd' _ _ _ _ m' -> m' == m
  ModMatMul' _ _ _ _ _ m' -> m' == m
  ModMatPow' _ _ _ m' -> m' == m
  ModSum' _ m' -> m' == m
  ModProduct' _ m' -> m' == m
  LitInt' n -> case m of
    LitInt' m -> 0 <= n && n < m
    _ -> False
  Proj' ts _ e | isVectorTy' ts -> e `isModulo'` Mod m
  Proj' ts _ e | isMatrixTy' ts -> e `isModulo'` Mod m
  Map' _ _ f _ -> f `isModulo'` Mod m
  Lam _ _ body -> body `isModulo'` Mod m
  e@(App _ _) -> case curryApp e of
    (e@(Lam _ _ _), _) -> e `isModulo'` Mod m
    (Tuple' ts, es) | isVectorTy' ts -> all (`isModulo'` Mod m) es
    (Tuple' ts, es) | isMatrixTy' ts -> all (`isModulo'` Mod m) es
    _ -> False
  _ -> False

isModulo :: Expr -> Expr -> Bool
isModulo e m = e `isModulo'` Mod m

putFloorMod :: MonadAlpha m => Mod -> Expr -> m (Maybe Expr)
putFloorMod (Mod m) =
  let return' = return . Just
   in \case
        Negate' e -> return' $ ModNegate' e m
        Plus' e1 e2 -> return' $ ModPlus' e1 e2 m
        Minus' e1 e2 -> return' $ ModMinus' e1 e2 m
        Mult' e1 e2 -> return' $ ModMult' e1 e2 m
        Pow' e1 e2 -> return' $ ModPow' e1 e2 m
        MatAp' h w e1 e2 -> return' $ ModMatAp' h w e1 e2 m
        MatAdd' h w e1 e2 -> return' $ ModMatAdd' h w e1 e2 m
        MatMul' h n w e1 e2 -> return' $ ModMatMul' h n w e1 e2 m
        MatPow' n e1 e2 -> return' $ ModMatPow' n e1 e2 m
        Sum' e -> return' $ ModSum' e m
        Product' e -> return' $ ModProduct' e m
        LitInt' n -> case m of
          LitInt' m -> return' $ LitInt' (n `mod` m)
          _ -> return Nothing
        Proj' ts i e | isVectorTy' ts -> return' $ Proj' ts i (VecFloorMod' (genericLength ts) e m)
        Proj' ts i e
          | isMatrixTy' ts ->
            let (h, w) = fromJust (sizeOfMatrixTy (TupleTy ts))
             in return' $ Proj' ts i (MatFloorMod' (toInteger h) (toInteger w) e m)
        Map' t1 t2 f xs -> do
          f <- putFloorMod (Mod m) f
          case f of
            Nothing -> return Nothing
            Just f -> return' $ Map' t1 t2 f xs
        Lam x t body -> do
          -- TODO: rename only if required
          y <- genVarName x
          body <- substitute x (Var y) body
          body <- putFloorMod (Mod m) body
          case body of
            Nothing -> return Nothing
            Just body -> return' $ Lam y t body
        e@(App _ _) -> case curryApp e of
          (f@(Lam _ _ _), args) -> do
            f <- putFloorMod (Mod m) f
            case f of
              Nothing -> return Nothing
              Just f -> return' $ uncurryApp f args
          (Tuple' ts, es) | isVectorTy' ts -> do
            es' <- mapM (putFloorMod (Mod m)) es
            if all isNothing es'
              then return Nothing
              else return' $ uncurryApp (Tuple' ts) (zipWith fromMaybe es es')
          (Tuple' ts, es) | isMatrixTy (TupleTy ts) -> do
            es' <- mapM (putFloorMod (Mod m)) es
            if all isNothing es'
              then return Nothing
              else return' $ uncurryApp (Tuple' ts) (zipWith fromMaybe es es')
          _ -> return Nothing
        _ -> return Nothing

putFloorModGeneric :: MonadAlpha m => (Expr -> Mod -> m Expr) -> Mod -> Expr -> m Expr
putFloorModGeneric fallback m e =
  if e `isModulo'` m
    then return e
    else do
      e' <- putFloorMod m e
      case e' of
        Just e' -> return e'
        Nothing -> fallback e m

putFloorModInt :: MonadAlpha m => Mod -> Expr -> m Expr
putFloorModInt = putFloorModGeneric (\e (Mod m) -> return $ FloorMod' e m)

putMapFloorMod :: MonadAlpha m => Mod -> Expr -> m Expr
putMapFloorMod = putFloorModGeneric fallback
  where
    fallback e (Mod m) = do
      x <- genVarName'
      return $ Map' IntTy IntTy (Lam x IntTy (FloorMod' (Var x) m)) e

putVecFloorMod :: (MonadError Error m, MonadAlpha m) => [(VarName, Type)] -> Mod -> Expr -> m Expr
putVecFloorMod env = putFloorModGeneric fallback
  where
    fallback e (Mod m) = do
      t <- typecheckExpr env e
      case t of
        TupleTy ts -> return $ VecFloorMod' (genericLength ts) e m
        _ -> throwInternalError $ "not a vector: " ++ formatType t

putMatFloorMod :: (MonadError Error m, MonadAlpha m) => [(VarName, Type)] -> Mod -> Expr -> m Expr
putMatFloorMod env = putFloorModGeneric fallback
  where
    fallback e (Mod m) = do
      t <- typecheckExpr env e
      case t of
        TupleTy ts@(TupleTy ts' : _) -> return $ MatFloorMod' (genericLength ts) (genericLength ts') e m
        _ -> throwInternalError $ "not a matrix: " ++ formatType t

rule :: (MonadAlpha m, MonadError Error m) => RewriteRule m
rule =
  let go1 m f (t1, e1) = Just <$> (f <$> t1 (Mod m) e1 <*> pure m)
      go2 m f (t1, e1) (t2, e2) = Just <$> (f <$> t1 (Mod m) e1 <*> t2 (Mod m) e2 <*> pure m)
   in makeRewriteRule "Jikka.Core.Convert.PropagateMod" $ \env -> \case
        ModNegate' e m | not (e `isModulo` m) -> go1 m ModNegate' (putFloorModInt, e)
        ModPlus' e1 e2 m | not (e1 `isModulo` m) || not (e2 `isModulo` m) -> go2 m ModPlus' (putFloorModInt, e1) (putFloorModInt, e2)
        ModMinus' e1 e2 m | not (e1 `isModulo` m) || not (e2 `isModulo` m) -> go2 m ModMinus' (putFloorModInt, e1) (putFloorModInt, e2)
        ModMult' e1 e2 m | not (e1 `isModulo` m) || not (e2 `isModulo` m) -> go2 m ModMult' (putFloorModInt, e1) (putFloorModInt, e2)
        ModInv' e m | not (e `isModulo` m) -> go1 m ModInv' (putFloorModInt, e)
        ModPow' e1 e2 m | not (e1 `isModulo` m) -> go2 m ModPow' (putFloorModInt, e1) (\_ e -> return e, e2)
        ModMatAp' h w e1 e2 m | not (e1 `isModulo` m) || not (e2 `isModulo` m) -> go2 m (ModMatAp' h w) (putMatFloorMod env, e1) (putVecFloorMod env, e2)
        ModMatAdd' h w e1 e2 m | not (e1 `isModulo` m) || not (e2 `isModulo` m) -> go2 m (ModMatAdd' h w) (putMatFloorMod env, e1) (putMatFloorMod env, e2)
        ModMatMul' h n w e1 e2 m | not (e1 `isModulo` m) || not (e2 `isModulo` m) -> go2 m (ModMatMul' h n w) (putMatFloorMod env, e1) (putMatFloorMod env, e2)
        ModMatPow' n e1 e2 m | not (e1 `isModulo` m) -> go2 m (ModMatPow' n) (putMatFloorMod env, e1) (\_ e -> return e, e2)
        ModSum' e m | not (e `isModulo` m) -> go1 m ModSum' (putMapFloorMod, e)
        ModProduct' e m | not (e `isModulo` m) -> go1 m ModProduct' (putMapFloorMod, e)
        FloorMod' e m ->
          if e `isModulo` m
            then return $ Just e
            else putFloorMod (Mod m) e
        VecFloorMod' _ e m ->
          if e `isModulo` m
            then return $ Just e
            else putFloorMod (Mod m) e
        MatFloorMod' _ _ e m ->
          if e `isModulo` m
            then return $ Just e
            else putFloorMod (Mod m) e
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
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
