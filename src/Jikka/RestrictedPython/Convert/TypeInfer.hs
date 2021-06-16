{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.RestrictedPython.Convert.TypeInfer
-- Description : infers types of the exprs of the restricted Python.
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Convert.TypeInfer
  ( run,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid (Dual(..))
import qualified Data.Map.Strict as M
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Expr

data Equation
  = TypeEquation Type Type
  | TypeAssertion Ident Type
  deriving (Eq, Ord, Show, Read)

type Eqns = Dual [Equation]

gensym :: MonadAlpha m => m Ident
gensym = do
  i <- nextCounter
  return $ Ident ('$' : show i)

genType :: MonadAlpha m => m Type
genType = VarTy <$> gensym

unifyType :: MonadWriter Eqns m => Type -> Type -> m ()
unifyType t1 t2 = tell $ Dual [TypeEquation t1 t2]

unifyIdent :: MonadWriter Eqns m => Ident -> Type -> m ()
unifyIdent x t = tell $ Dual [TypeAssertion x t]

unifyTarget :: (MonadWriter Eqns m, MonadAlpha m) => Target -> m Type
unifyTarget = \case
  SubscriptTrg x t indices -> do
    unifyIdent x t
    let go t = \case
                  [] -> return t
                  (e : es) -> do
                    unifyExpr' e IntTy
                    t' <- genType
                    unifyType t (ListTy t')
                    go t' es
    go t indices
  NameTrg x t -> do
    unifyIdent x t
    return t
  TupleTrg xts -> do
    mapM_ (uncurry unifyIdent) xts
    return $ TupleTy (map snd xts)

unifyExpr :: (MonadWriter Eqns m, MonadAlpha m) => Expr -> m Type
unifyExpr = \case
    BoolOp e1 _ e2 -> do
      unifyExpr' e1 BoolTy
      unifyExpr' e2 BoolTy
      return BoolTy
    BinOp e1 _ e2 -> do
      unifyExpr' e1 IntTy
      unifyExpr' e2 IntTy
      return IntTy
    UnaryOp op e -> do
      let t' = if op == Not then BoolTy else IntTy
      unifyExpr' e t'
      return t'
    Lambda args body -> do
      mapM_ (uncurry unifyIdent) args
      ret <- genType
      unifyExpr body
      return $ CallableTy (map snd args) ret
    IfExp e1 e2 e3 -> do
      t1 <- unifyExpr e1
      t2 <- unifyExpr e2
      t3 <- unifyExpr e3
      unifyType t1 BoolTy
      unifyType t2 t3
      return t2
    ListComp e comp -> do
      let Comprehension x iter pred = comp
      te <- unifyExpr e
      tx <- unifyTarget x
      titer <- unifyExpr iter
      unifyType (ListTy tx) titer
      case pred of
        Nothing -> return ()
        Just pred -> do
          tpred <- unifyExpr pred
          unifyType tpred BoolTy
      return $ ListTy te
    Compare e1 _ e2 -> do
      t1 <- unifyExpr e1
      t2 <- unifyExpr e2
      unifyType t1 t2
      return BoolTy
    Call f args -> do
      ts <- mapM unifyExpr args
      ret <- genType
      unifyExpr' f (CallableTy ts ret)
      return ret
    Constant const ->
      return $ case const of
        ConstNone -> TupleTy []
        ConstInt _ -> IntTy
        ConstBool _ -> BoolTy
    Subscript e1 e2 -> do
      t <- genType
      unifyExpr' e1 (ListTy t)
      unifyExpr' e2 IntTy
      return t
    Name x -> do
      t <- genType
      unifyIdent x t
      return t
    List t es -> do
      forM_ es $ \e -> do
        unifyExpr' e t
      return $ ListTy t
    Tuple es -> TupleTy <$> (mapM unifyExpr) es
    SubscriptSlice e from to step -> do
      t' <- genType
      unifyExpr' e (ListTy t')
      let unify = \case
            Nothing -> return ()
            Just e -> unifyExpr' e IntTy
      unify from
      unify to
      unify step
      return t'

unifyExpr' :: (MonadWriter Eqns m, MonadAlpha m) => Expr -> Type -> m ()
unifyExpr' e t = do
  t' <- unifyExpr e
  unifyType t t'

unifyStatement :: (MonadWriter Eqns m, MonadAlpha m) => Type -> Statement -> m ()
unifyStatement ret = \case
  Return e -> do
    t <- unifyExpr e
    unifyType t ret
  AugAssign x _ e -> do
    t1 <- unifyTarget x
    t2 <- unifyExpr e
    unifyType t1 IntTy
    unifyType t2 IntTy
  AnnAssign x e -> do
    t1 <- unifyTarget x
    t2 <- unifyExpr e
    unifyType t1 t2
  For x e body -> do
    t1 <- unifyTarget x
    t2 <- unifyExpr e
    unifyType (ListTy t1) t2
    mapM_ (unifyStatement ret) body
  If e body1 body2 -> do
    t <- unifyExpr e
    unifyType t BoolTy
    mapM_ (unifyStatement ret) body1
    mapM_ (unifyStatement ret) body2
  Assert e -> do
    t <- unifyExpr e
    unifyType t BoolTy

unifyToplevelStatement :: (MonadWriter Eqns m, MonadAlpha m) => ToplevelStatement -> m ()
unifyToplevelStatement = \case
  ToplevelAnnAssign x t e -> do
    unifyIdent x t
    unifyExpr' e t
  ToplevelFunctionDef f args ret body -> do
    mapM_ (uncurry unifyIdent) args
    unifyIdent f (CallableTy (map snd args) ret)
    mapM_ (unifyStatement ret) body
  ToplevelAssert e -> do
    t <- unifyExpr e
    unifyType t BoolTy

unifyProgram :: (MonadWriter Eqns m, MonadAlpha m) => Program -> m ()
unifyProgram prog = mapM_ unifyToplevelStatement prog

-- | `Subst` is type substituion. It's a mapping from type variables to their actual types.
newtype Subst = Subst (M.Map Ident Type)

-- | `Env` is type environments. It's a mapping from variables to their types.
newtype Env = Env (M.Map Ident Type)

solveEquations :: MonadError Error m => Eqns -> m (Subst, Env)
solveEquations = undefined

-- subst :: MonadEquationWriter m => Type -> m Type
-- subst = \case
--   VarTy x -> do
--     f <- get
--     return $ case M.lookup x f of
--       Nothing -> VarTy x
--       Just t -> t
--   IntTy -> return IntTy
--   BoolTy -> return BoolTy
--   ListTy t -> ListTy <$> subst t
--   TupleTy ts -> TupleTy <$> mapM susbt ts
--   CallableTy ts ret -> CallableTy <$> mapM subst ts <*> subst ret


-- freeTyVars :: Type -> [Ident]
-- freeTyVars = \case
--   VarTy x -> [x]
--   IntTy -> []
--   BoolTy -> []
--   ListTy t -> freeTyVars t
--   TupleTy ts -> mapM freeTyVars ts
--   CallableTy ts ret -> mapM freeTyVars (ret : ts)
-- 
-- unifyTyVar :: (MonadEquationWriter m, MonadError Error m) => Ident -> Type -> m ()
-- unifyTyVar x t =
--   if x `elem` freeTyVars t
--     then throwTypeError $ "looped type equation " ++ show x ++ " = " show t
--     else do
--       modify' (M.insert x t)  -- This doesn't introduce the loop.
-- 
-- unifyType :: (MonadEquationWriter m, MonadError Error m) => Type -> Type -> m Type
-- unifyType t1 t2 = wrapError' ("failed to unify " ++ show t1 ++ " and " ++ show t2) $ go t1 t2 where
--   go :: (MonadEquationWriter m, MonadError Error m) => Type -> Type -> m Type
--   go t1 t2 = do
--     t1 <- subst t1
--     t2 <- subst t2
--     case (t1, t2) of
--       _ | t1 == t2 -> do
--         return t1
--       (VarTy x1, _) -> do
--         unifyTyVar x1 t2
--         return $ VarTy x1
--       (_, VarTy x2) -> do
--         unifyTyVar x2 t1
--         return $ VarTy x2
--       (ListTy t1, ListTy t2) -> do
--         t <- go t1 t2
--         return $ ListTy t
--       (TupleTy ts1, TupleTy ts2) -> do
--         ts <- mapM go ts1 ts2
--         return $ TupleTy ts
--       (CallableTy args1 ret1, CallableTy args2 ret2) -> do
--         args <- mapM go args1 args2
--         ret <- go ret1 ret2
--         return $ CallableTy args ret
--       _ -> throwTypeError $ "different types " ++ show t1 ++ " /= " ++ show t2

-- | `run` infers types of given programs.
--
-- There must be no name conflicts in given programs. They must be alpha-converted.
--
-- As its interface, this function works as follows:
--
-- 1. Finds a type environment \(\Gamma\) s.t. for all statement \(\mathrm{stmt}\) in the given program, \(\Gamma \vdash \mathrm{stmt}\) holds, and
-- 2. Annotates each variable in the program using the \(\Gamma\).
--
-- In its implementation, this function works like a Hindley-Milner type inference.
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run = do
  undefined
