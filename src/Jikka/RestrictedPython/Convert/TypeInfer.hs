{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.RestrictedPython.Convert.TypeInfer
-- Description : does type inference. / 型推論を行います。
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
    mapTypeProgram,
  )
where

import Control.Arrow (second)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as M
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Format (formatType)
import Jikka.RestrictedPython.Language.Builtin
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util

data Equation
  = TypeEquation Type Type (Maybe Loc)
  | TypeAssertion VarName' Type
  deriving (Eq, Ord, Show, Read)

type Eqns = Dual [Equation]

formularizeType :: MonadWriter Eqns m => Type -> Type -> Maybe Loc -> m ()
formularizeType t1 t2 location = tell $ Dual [TypeEquation t1 t2 location]

formularizeVarName :: MonadWriter Eqns m => VarName' -> Type -> m ()
formularizeVarName x t = tell $ Dual [TypeAssertion x t]

formularizeTarget :: (MonadWriter Eqns m, MonadAlpha m) => Target' -> m Type
formularizeTarget x0 = case value' x0 of
  SubscriptTrg f index -> do
    t <- genType
    tf <- formularizeTarget f
    formularizeType tf (ListTy t) (loc' x0)
    tindex <- formularizeExpr index
    formularizeType tindex IntTy (loc' x0)
    return t
  NameTrg x -> do
    t <- genType
    formularizeVarName x t
    return t
  TupleTrg xs -> do
    TupleTy <$> mapM formularizeTarget xs

formularizeTarget' :: (MonadWriter Eqns m, MonadAlpha m) => Target' -> Type -> m ()
formularizeTarget' x0 t = do
  t' <- formularizeTarget x0
  formularizeType t t' (loc' x0)

formularizeExpr :: (MonadWriter Eqns m, MonadAlpha m) => Expr' -> m Type
formularizeExpr e0 = case value' e0 of
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
    formularizeExpr' body ret
    return $ CallableTy (map snd args) ret
  IfExp e1 e2 e3 -> do
    formularizeExpr' e1 BoolTy
    t <- formularizeExpr e2
    formularizeExpr' e3 t
    return t
  ListComp e comp -> do
    let Comprehension x iter pred = comp
    te <- formularizeExpr e
    tx <- formularizeTarget x
    formularizeExpr' iter (ListTy tx)
    case pred of
      Nothing -> return ()
      Just pred -> formularizeExpr' pred BoolTy
    return $ ListTy te
  Compare e1 (CmpOp' op t) e2 -> do
    formularizeExpr' e1 t
    formularizeExpr' e2 (if op == In || op == NotIn then ListTy t else t)
    return BoolTy
  Call f args -> do
    ts <- mapM formularizeExpr args
    ret <- genType
    formularizeExpr' f (CallableTy ts ret)
    return ret
  Constant const ->
    return $ case const of
      ConstNone -> NoneTy
      ConstInt _ -> IntTy
      ConstBool _ -> BoolTy
      ConstBuiltin b -> typeBuiltin b
  Attribute e x -> do
    let (t1, t2) = typeAttribute (value' x)
    formularizeExpr' e t1
    return t2
  Subscript e1 e2 -> do
    t <- genType
    formularizeExpr' e1 (ListTy t)
    formularizeExpr' e2 IntTy
    return t
  Starred e -> do
    t <- genType
    formularizeExpr' e (ListTy t)
    return t -- because @*xs@ and @y@ has the same type in @[*xs, y]@
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
    return (ListTy t')

formularizeExpr' :: (MonadWriter Eqns m, MonadAlpha m) => Expr' -> Type -> m ()
formularizeExpr' e0 t = do
  t' <- formularizeExpr e0
  formularizeType t t' (loc' e0)

formularizeStatement :: (MonadWriter Eqns m, MonadAlpha m) => Type -> Statement -> m ()
formularizeStatement ret = \case
  Return e -> do
    t <- formularizeExpr e
    formularizeType t ret (loc' e)
  AugAssign x _ e -> do
    formularizeTarget' x IntTy
    formularizeExpr' e IntTy
  AnnAssign x t e -> do
    formularizeTarget' x t
    formularizeExpr' e t
  For x e body -> do
    t <- formularizeTarget x
    formularizeExpr' e (ListTy t)
    mapM_ (formularizeStatement ret) body
  If e body1 body2 -> do
    formularizeExpr' e BoolTy
    mapM_ (formularizeStatement ret) body1
    mapM_ (formularizeStatement ret) body2
  Assert e -> do
    formularizeExpr' e BoolTy
  Expr' e -> do
    formularizeExpr' e SideEffectTy

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
    formularizeExpr' e BoolTy

formularizeProgram :: MonadAlpha m => Program -> m [Equation]
formularizeProgram prog = getDual <$> execWriterT (mapM_ formularizeToplevelStatement prog)

sortEquations :: [Equation] -> ([(Type, Type, Maybe Loc)], [(VarName', Type)])
sortEquations = go [] []
  where
    go eqns' assertions [] = (eqns', assertions)
    go eqns' assertions (eqn : eqns) = case eqn of
      TypeEquation t1 t2 loc -> go ((t1, t2, loc) : eqns') assertions eqns
      TypeAssertion x t -> go eqns' ((x, t) : assertions) eqns

mergeAssertions :: [(VarName', Type)] -> [(Type, Type, Maybe Loc)]
mergeAssertions = go M.empty []
  where
    go _ eqns [] = eqns
    go gamma eqns ((x, t) : assertions) = case M.lookup (value' x) gamma of
      Nothing -> go (M.insert (value' x) t gamma) eqns assertions
      Just t' -> go gamma ((t, t', loc' x) : eqns) assertions

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
  StringTy -> StringTy
  SideEffectTy -> SideEffectTy

unifyTyVar :: (MonadState Subst m, MonadError Error m) => TypeName -> Type -> m ()
unifyTyVar x t =
  if x `elem` freeTyVars t
    then throwTypeError $ "type equation loops: " ++ formatType (VarTy x) ++ " = " ++ formatType t
    else do
      modify' (Subst . M.insert x t . unSubst) -- This doesn't introduce the loop.

unifyType :: (MonadState Subst m, MonadError Error m) => Type -> Type -> m ()
unifyType t1 t2 = do
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
        else throwTypeError $ "type " ++ formatType t1 ++ " is not type " ++ formatType t2
    (CallableTy args1 ret1, CallableTy args2 ret2) -> do
      if length args1 == length args2
        then mapM_ (uncurry unifyType) (zip args1 args2)
        else throwTypeError $ "type " ++ formatType t1 ++ " is not type " ++ formatType t2
      unifyType ret1 ret2
    _ -> throwTypeError $ "type " ++ formatType t1 ++ " is not type " ++ formatType t2

solveEquations :: MonadError Error m => [(Type, Type, Maybe Loc)] -> m Subst
solveEquations eqns = wrapError' "failed to solve type equations" $ do
  flip execStateT (Subst M.empty) $ do
    errs <- forM eqns $ \(t1, t2, loc) -> do
      (Right <$> unifyType t1 t2) `catchError` \err -> do
        sigma <- get
        t1 <- return $ subst sigma t1 -- shadowing
        t2 <- return $ subst sigma t2 -- shadowing
        return $ Left (maybe id WithLocation loc (WithWrapped ("failed to unify type " ++ formatType t1 ++ " and type " ++ formatType t2) err))
    reportErrors errs

mapTypeConstant :: (Type -> Type) -> Constant -> Constant
mapTypeConstant f = \case
  ConstNone -> ConstNone
  ConstInt n -> ConstInt n
  ConstBool p -> ConstBool p
  ConstBuiltin b -> ConstBuiltin (mapTypeBuiltin f b)

mapTypeTarget :: (Type -> Type) -> Target' -> Target'
mapTypeTarget f = fmap $ \case
  SubscriptTrg x index -> SubscriptTrg (mapTypeTarget f x) (mapTypeExpr f index)
  NameTrg x -> NameTrg x
  TupleTrg xs -> TupleTrg (map (mapTypeTarget f) xs)

mapTypeExpr :: (Type -> Type) -> Expr' -> Expr'
mapTypeExpr f = mapSubExpr go
  where
    go = fmap $ \case
      Lambda args body -> Lambda (map (second f) args) (go body)
      ListComp e (Comprehension x iter pred) -> ListComp (go e) (Comprehension (mapTypeTarget f x) (go iter) (fmap go pred))
      Compare e1 (CmpOp' op t) e2 -> Compare (go e1) (CmpOp' op (f t)) (go e2)
      Constant const -> Constant (mapTypeConstant f const)
      Attribute e a -> Attribute (go e) (mapTypeAttribute f <$> a)
      List t es -> List (f t) (map go es)
      e -> e

mapTypeStatement :: (Type -> Type) -> Statement -> Statement
mapTypeStatement f = \case
  Return e -> Return (mapTypeExpr f e)
  AugAssign x op e -> AugAssign (mapTypeTarget f x) op (mapTypeExpr f e)
  AnnAssign x t e -> AnnAssign (mapTypeTarget f x) (f t) (mapTypeExpr f e)
  For x iter body -> For (mapTypeTarget f x) (mapTypeExpr f iter) (map (mapTypeStatement f) body)
  If pred body1 body2 -> If (mapTypeExpr f pred) (map (mapTypeStatement f) body1) (map (mapTypeStatement f) body2)
  Assert e -> Assert (mapTypeExpr f e)
  Expr' e -> Expr' (mapTypeExpr f e)

mapTypeToplevelStatement :: (Type -> Type) -> ToplevelStatement -> ToplevelStatement
mapTypeToplevelStatement f = \case
  ToplevelAnnAssign x t e -> ToplevelAnnAssign x (f t) (mapTypeExpr f e)
  ToplevelFunctionDef g args ret body -> ToplevelFunctionDef g (map (second f) args) (f ret) (map (mapTypeStatement f) body)
  ToplevelAssert e -> ToplevelAssert (mapTypeExpr f e)

mapTypeProgram :: (Type -> Type) -> Program -> Program
mapTypeProgram f prog = map (mapTypeToplevelStatement f) prog

-- | `substUnit` replaces all undetermined type variables with the unit type.
substUnit :: Type -> Type
substUnit = \case
  VarTy _ -> NoneTy
  IntTy -> IntTy
  BoolTy -> BoolTy
  ListTy t -> ListTy (substUnit t)
  TupleTy ts -> TupleTy (map substUnit ts)
  CallableTy ts ret -> CallableTy (map substUnit ts) (substUnit ret)
  StringTy -> StringTy
  SideEffectTy -> SideEffectTy

-- | `subst'` does `subst` and replaces all undetermined type variables with the unit type.
subst' :: Subst -> Type -> Type
subst' sigma = substUnit . subst sigma

-- | `run` infers types of given programs.
--
-- As the interface, you can understand this function does the following:
--
-- 1. Finds a type environment \(\Gamma\) s.t. for all statement \(\mathrm{stmt}\) in the given program, \(\Gamma \vdash \mathrm{stmt}\) holds, and
-- 2. Annotates each variable in the program using the \(\Gamma\).
--
-- In its implementation, this is just something like a Hindley-Milner type inference.
--
-- == Requirements
--
-- * There must be no name conflicts in given programs. They must be alpha-converted. (`Jikka.RestrictedPython.Convert.Alpha`)
-- * All names must be resolved. (`Jikka.RestrictedPython.Convert.ResolveBuiltin`)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.RestrictedPython.Convert.TypeInfer" $ do
  eqns <- formularizeProgram prog
  let (eqns', assertions) = sortEquations eqns
  let eqns'' = mergeAssertions assertions
  sigma <- solveEquations (eqns' ++ eqns'')
  return $ mapTypeProgram (subst' sigma) prog
