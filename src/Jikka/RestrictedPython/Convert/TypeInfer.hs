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

    -- * internal types and functions
    Equation (..),
    formularizeProgram,
    sortEquations,
    mergeAssertions,
    Subst (..),
    subst,
    solveEquations,
    substProgram,
  )
where

import Control.Arrow (second)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as M
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Builtin
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util

data Equation
  = TypeEquation Type Type
  | TypeAssertion VarName' Type
  deriving (Eq, Ord, Show, Read)

type Eqns = Dual [Equation]

formularizeType :: MonadWriter Eqns m => Type -> Type -> m ()
formularizeType t1 t2 = tell $ Dual [TypeEquation t1 t2]

formularizeVarName :: MonadWriter Eqns m => VarName' -> Type -> m ()
formularizeVarName x t = tell $ Dual [TypeAssertion x t]

formularizeTarget :: (MonadWriter Eqns m, MonadAlpha m) => Target -> m Type
formularizeTarget = \case
  SubscriptTrg f index -> do
    t <- genType
    tf <- formularizeTarget f
    formularizeType tf (ListTy t)
    tindex <- formularizeExpr index
    formularizeType tindex IntTy
    return t
  NameTrg x -> do
    t <- genType
    formularizeVarName x t
    return t
  TupleTrg xs -> do
    TupleTy <$> mapM formularizeTarget xs

formularizeExpr :: (MonadWriter Eqns m, MonadAlpha m) => Expr -> m Type
formularizeExpr = \case
  BoolOp e1 _ e2 -> do
    formularizeExpr' e1 BoolTy
    formularizeExpr' e2 BoolTy
    return BoolTy
  BinOp e1 _ e2 -> do
    formularizeExpr' e1 IntTy
    formularizeExpr' e2 IntTy
    return IntTy
  UnaryOp op e -> do
    let t' = if op == Not then BoolTy else IntTy
    formularizeExpr' e t'
    return t'
  Lambda args body -> do
    mapM_ (uncurry formularizeVarName) args
    ret <- genType
    formularizeExpr body
    return $ CallableTy (map snd args) ret
  IfExp e1 e2 e3 -> do
    t1 <- formularizeExpr e1
    t2 <- formularizeExpr e2
    t3 <- formularizeExpr e3
    formularizeType t1 BoolTy
    formularizeType t2 t3
    return t2
  ListComp e comp -> do
    let Comprehension x iter pred = comp
    te <- formularizeExpr e
    tx <- formularizeTarget x
    titer <- formularizeExpr iter
    formularizeType (ListTy tx) titer
    case pred of
      Nothing -> return ()
      Just pred -> do
        tpred <- formularizeExpr pred
        formularizeType tpred BoolTy
    return $ ListTy te
  Compare e1 (CmpOp' op t) e2 -> do
    t1 <- formularizeExpr e1
    t2 <- formularizeExpr e2
    formularizeType t1 t
    formularizeType t2 (if op == In || op == NotIn then ListTy t else t)
    return BoolTy
  Call f args -> do
    ts <- mapM formularizeExpr args
    ret <- genType
    formularizeExpr' f (CallableTy ts ret)
    return ret
  Constant const ->
    return $ case const of
      ConstNone -> TupleTy []
      ConstInt _ -> IntTy
      ConstBool _ -> BoolTy
      ConstBuiltin b -> typeBuiltin b
  Subscript e1 e2 -> do
    t <- genType
    formularizeExpr' e1 (ListTy t)
    formularizeExpr' e2 IntTy
    return t
  Name x -> do
    t <- genType
    formularizeVarName x t
    return t
  List t es -> do
    forM_ es $ \e -> do
      formularizeExpr' e t
    return $ ListTy t
  Tuple es -> TupleTy <$> mapM formularizeExpr es
  SubscriptSlice e from to step -> do
    t' <- genType
    formularizeExpr' e (ListTy t')
    let formularize = \case
          Nothing -> return ()
          Just e -> formularizeExpr' e IntTy
    formularize from
    formularize to
    formularize step
    return t'

formularizeExpr' :: (MonadWriter Eqns m, MonadAlpha m) => Expr -> Type -> m ()
formularizeExpr' e t = do
  t' <- formularizeExpr e
  formularizeType t t'

formularizeStatement :: (MonadWriter Eqns m, MonadAlpha m) => Type -> Statement -> m ()
formularizeStatement ret = \case
  Return e -> do
    t <- formularizeExpr e
    formularizeType t ret
  AugAssign x _ e -> do
    t1 <- formularizeTarget x
    t2 <- formularizeExpr e
    formularizeType t1 IntTy
    formularizeType t2 IntTy
  AnnAssign x t e -> do
    t1 <- formularizeTarget x
    t2 <- formularizeExpr e
    formularizeType t1 t
    formularizeType t2 t
  For x e body -> do
    t1 <- formularizeTarget x
    t2 <- formularizeExpr e
    formularizeType (ListTy t1) t2
    mapM_ (formularizeStatement ret) body
  If e body1 body2 -> do
    t <- formularizeExpr e
    formularizeType t BoolTy
    mapM_ (formularizeStatement ret) body1
    mapM_ (formularizeStatement ret) body2
  Assert e -> do
    t <- formularizeExpr e
    formularizeType t BoolTy

formularizeToplevelStatement :: (MonadWriter Eqns m, MonadAlpha m) => ToplevelStatement -> m ()
formularizeToplevelStatement = \case
  ToplevelAnnAssign x t e -> do
    formularizeVarName x t
    formularizeExpr' e t
  ToplevelFunctionDef f args ret body -> do
    mapM_ (uncurry formularizeVarName) args
    formularizeVarName f (CallableTy (map snd args) ret)
    mapM_ (formularizeStatement ret) body
  ToplevelAssert e -> do
    t <- formularizeExpr e
    formularizeType t BoolTy

formularizeProgram :: MonadAlpha m => Program -> m [Equation]
formularizeProgram prog = getDual <$> execWriterT (mapM_ formularizeToplevelStatement prog)

sortEquations :: [Equation] -> ([(Type, Type)], [(VarName', Type)])
sortEquations = go [] []
  where
    go eqns' assertions [] = (eqns', assertions)
    go eqns' assertions (eqn : eqns) = case eqn of
      TypeEquation t1 t2 -> go ((t1, t2) : eqns') assertions eqns
      TypeAssertion x t -> go eqns' ((x, t) : assertions) eqns

mergeAssertions :: [(VarName', Type)] -> [(Type, Type)]
mergeAssertions = go M.empty []
  where
    go _ eqns [] = eqns
    go gamma eqns ((x, t) : assertions) = case M.lookup x gamma of
      Nothing -> go (M.insert x t gamma) eqns assertions
      Just t' -> go gamma ((t, t') : eqns) assertions

-- | `Subst` is type substituion. It's a mapping from type variables to their actual types.
newtype Subst = Subst {unSubst :: M.Map TypeName Type}

subst :: Subst -> Type -> Type
subst sigma = \case
  VarTy x ->
    case M.lookup x (unSubst sigma) of
      Nothing -> VarTy x
      Just t -> subst sigma t
  IntTy -> IntTy
  BoolTy -> BoolTy
  ListTy t -> ListTy (subst sigma t)
  TupleTy ts -> TupleTy (map (subst sigma) ts)
  CallableTy ts ret -> CallableTy (map (subst sigma) ts) (subst sigma ret)

unifyTyVar :: (MonadState Subst m, MonadError Error m) => TypeName -> Type -> m ()
unifyTyVar x t =
  if x `elem` freeTyVars t
    then throwTypeError $ "looped type equation " ++ show x ++ " = " ++ show t
    else do
      modify' (Subst . M.insert x t . unSubst) -- This doesn't introduce the loop.

unifyType :: (MonadState Subst m, MonadError Error m) => Type -> Type -> m ()
unifyType t1 t2 = wrapError' ("failed to unify " ++ show t1 ++ " and " ++ show t2) $ do
  sigma <- get
  t1 <- return $ subst sigma t1 -- shadowing
  t2 <- return $ subst sigma t2 -- shadowing
  case (t1, t2) of
    _ | t1 == t2 -> return ()
    (VarTy x1, _) -> do
      unifyTyVar x1 t2
    (_, VarTy x2) -> do
      unifyTyVar x2 t1
    (ListTy t1, ListTy t2) -> do
      unifyType t1 t2
    (TupleTy ts1, TupleTy ts2) -> do
      if length ts1 == length ts2
        then mapM_ (uncurry unifyType) (zip ts1 ts2)
        else throwTypeError $ "different types " ++ show t1 ++ " /= " ++ show t2
    (CallableTy args1 ret1, CallableTy args2 ret2) -> do
      if length args1 == length args2
        then mapM_ (uncurry unifyType) (zip args1 args2)
        else throwTypeError $ "different types " ++ show t1 ++ " /= " ++ show t2
      unifyType ret1 ret2
    _ -> throwTypeError $ "different types " ++ show t1 ++ " /= " ++ show t2

solveEquations :: MonadError Error m => [(Type, Type)] -> m Subst
solveEquations eqns = wrapError' "failed to solve type equations" $ do
  execStateT (mapM_ (uncurry unifyType) eqns) (Subst M.empty)

-- | `substUnit` replaces all undetermined type variables with the unit type.
substUnit :: Type -> Type
substUnit = \case
  VarTy _ -> TupleTy []
  IntTy -> IntTy
  BoolTy -> BoolTy
  ListTy t -> ListTy (substUnit t)
  TupleTy ts -> TupleTy (map substUnit ts)
  CallableTy ts ret -> CallableTy (map substUnit ts) (substUnit ret)

-- | `subst'` does `subst` and replaces all undetermined type variables with the unit type.
subst' :: Subst -> Type -> Type
subst' sigma = substUnit . subst sigma

substTarget :: Subst -> Target -> Target
substTarget sigma = \case
  SubscriptTrg f index -> SubscriptTrg (substTarget sigma f) (substExpr sigma index)
  NameTrg x -> NameTrg x
  TupleTrg xs -> TupleTrg (map (substTarget sigma) xs)

substExpr :: Subst -> Expr -> Expr
substExpr sigma = go
  where
    go = \case
      BoolOp e1 op e2 -> BoolOp (go e1) op (go e2)
      BinOp e1 op e2 -> BinOp (go e1) op (go e2)
      UnaryOp op e -> UnaryOp op (go e)
      Lambda args body -> Lambda (map (second (subst' sigma)) args) (go body)
      IfExp e1 e2 e3 -> IfExp (go e1) (go e2) (go e3)
      ListComp e (Comprehension x iter pred) -> ListComp (go e) (Comprehension (substTarget sigma x) (go iter) (fmap go pred))
      Compare e1 op e2 -> Compare (go e1) op (go e2)
      Call f args -> Call (go f) (map go args)
      Constant const -> Constant const
      Subscript e1 e2 -> Subscript (go e1) (go e2)
      Name x -> Name x
      List t es -> List (subst' sigma t) (map go es)
      Tuple es -> Tuple (map go es)
      SubscriptSlice e from to step -> SubscriptSlice (go e) (fmap go from) (fmap go to) (fmap go step)

substStatement :: Subst -> Statement -> Statement
substStatement sigma = \case
  Return e -> Return (substExpr sigma e)
  AugAssign x op e -> AugAssign (substTarget sigma x) op (substExpr sigma e)
  AnnAssign x t e -> AnnAssign (substTarget sigma x) (subst' sigma t) (substExpr sigma e)
  For x iter body -> For (substTarget sigma x) (substExpr sigma iter) (map (substStatement sigma) body)
  If pred body1 body2 -> If (substExpr sigma pred) (map (substStatement sigma) body1) (map (substStatement sigma) body2)
  Assert e -> Assert (substExpr sigma e)

substToplevelStatement :: Subst -> ToplevelStatement -> ToplevelStatement
substToplevelStatement sigma = \case
  ToplevelAnnAssign x t e -> ToplevelAnnAssign x (subst' sigma t) (substExpr sigma e)
  ToplevelFunctionDef f args ret body -> ToplevelFunctionDef f (map (second (subst' sigma)) args) (subst' sigma ret) (map (substStatement sigma) body)
  ToplevelAssert e -> ToplevelAssert (substExpr sigma e)

substProgram :: Subst -> Program -> Program
substProgram sigma prog = map (substToplevelStatement sigma) prog

-- | `run` infers types of given programs.
--
-- There must be no name conflicts in given programs. They must be alpha-converted.
--
-- As the interface, you can understand this function does the following:
--
-- 1. Finds a type environment \(\Gamma\) s.t. for all statement \(\mathrm{stmt}\) in the given program, \(\Gamma \vdash \mathrm{stmt}\) holds, and
-- 2. Annotates each variable in the program using the \(\Gamma\).
--
-- In its implementation, this is just something like a Hindley-Milner type inference.
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.RestrictedPython.Convert.TypeInfer" $ do
  eqns <- formularizeProgram prog
  let (eqns', assertions) = sortEquations eqns
  let eqns'' = mergeAssertions assertions
  sigma <- solveEquations (eqns' ++ eqns'')
  return $ substProgram sigma prog
