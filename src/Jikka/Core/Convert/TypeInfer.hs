{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Convert.TypeInfer
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
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict (MonadWriter, execWriterT, tell)
import qualified Data.Map.Strict as M
import Data.Monoid (Dual (..))
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.TypeCheck (literalToType, typecheckProgram)
import Jikka.Core.Language.Util
import Jikka.Core.Language.Vars

data Equation
  = TypeEquation Type Type
  | TypeAssertion VarName Type
  deriving (Eq, Ord, Show, Read)

type Eqns = Dual [Equation]

formularizeType :: MonadWriter Eqns m => Type -> Type -> m ()
formularizeType t1 t2 = tell $ Dual [TypeEquation t1 t2]

formularizeVarName :: MonadWriter Eqns m => VarName -> Type -> m ()
formularizeVarName x t = tell $ Dual [TypeAssertion x t]

formularizeExpr :: (MonadWriter Eqns m, MonadAlpha m) => Expr -> m Type
formularizeExpr = \case
  Var x -> do
    t <- genType
    formularizeVarName x t
    return t
  Lit lit -> return $ literalToType lit
  App f args -> do
    ret <- genType
    t <- formularizeExpr f
    ts <- mapM formularizeExpr args
    formularizeType (FunTy ts ret) t
    return ret
  Lam args body -> do
    mapM_ (uncurry formularizeVarName) args
    ret <- formularizeExpr body
    return $ FunTy (map snd args) ret
  Let x t e1 e2 -> do
    formularizeVarName x t
    formularizeExpr' e1 t
    formularizeExpr e2

formularizeExpr' :: (MonadWriter Eqns m, MonadAlpha m) => Expr -> Type -> m ()
formularizeExpr' e t = do
  t' <- formularizeExpr e
  formularizeType t t'

formularizeToplevelExpr :: (MonadWriter Eqns m, MonadAlpha m) => ToplevelExpr -> m Type
formularizeToplevelExpr = \case
  ResultExpr e -> formularizeExpr e
  ToplevelLet x t e cont -> do
    formularizeVarName x t
    formularizeExpr' e t
    formularizeToplevelExpr cont
  ToplevelLetRec f args ret body cont -> do
    formularizeVarName f (FunTy (map snd args) ret)
    mapM_ (uncurry formularizeVarName) args
    formularizeExpr' body ret
    formularizeToplevelExpr cont

formularizeProgram :: MonadAlpha m => Program -> m [Equation]
formularizeProgram prog = getDual <$> execWriterT (formularizeToplevelExpr prog)

sortEquations :: [Equation] -> ([(Type, Type)], [(VarName, Type)])
sortEquations = go [] []
  where
    go eqns' assertions [] = (eqns', assertions)
    go eqns' assertions (eqn : eqns) = case eqn of
      TypeEquation t1 t2 -> go ((t1, t2) : eqns') assertions eqns
      TypeAssertion x t -> go eqns' ((x, t) : assertions) eqns

mergeAssertions :: [(VarName, Type)] -> [(Type, Type)]
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
  FunTy ts ret -> FunTy (map (subst sigma) ts) (subst sigma ret)

unifyTyVar :: (MonadState Subst m, MonadError Error m) => TypeName -> Type -> m ()
unifyTyVar x t =
  if x `elem` freeTyVars t
    then throwInternalError $ "looped type equation " ++ show x ++ " = " ++ show t
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
        else throwInternalError $ "different types " ++ show t1 ++ " /= " ++ show t2
    (FunTy args1 ret1, FunTy args2 ret2) -> do
      if length args1 == length args2
        then mapM_ (uncurry unifyType) (zip args1 args2)
        else throwInternalError $ "different types " ++ show t1 ++ " /= " ++ show t2
      unifyType ret1 ret2
    _ -> throwInternalError $ "different types " ++ show t1 ++ " /= " ++ show t2

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
  FunTy ts ret -> FunTy (map substUnit ts) (substUnit ret)

-- | `subst'` does `subst` and replaces all undetermined type variables with the unit type.
subst' :: Subst -> Type -> Type
subst' sigma = substUnit . subst sigma

substBuiltin :: Subst -> Builtin -> Builtin
substBuiltin sigma = mapTypeInBuiltin (subst' sigma)

substLiteral :: Subst -> Literal -> Literal
substLiteral sigma = \case
  LitBuiltin builtin -> LitBuiltin (substBuiltin sigma builtin)
  LitInt n -> LitInt n
  LitBool p -> LitBool p
  LitNil t -> LitNil (subst' sigma t)

substExpr :: Subst -> Expr -> Expr
substExpr sigma = go
  where
    go = \case
      Var x -> Var x
      Lit lit -> Lit (substLiteral sigma lit)
      App f args -> App (go f) (map go args)
      Lam args body -> Lam (map (second (subst' sigma)) args) (go body)
      Let x t e1 e2 -> Let x (subst sigma t) (go e1) (go e2)

substToplevelExpr :: Subst -> ToplevelExpr -> ToplevelExpr
substToplevelExpr sigma = \case
  ResultExpr e -> ResultExpr (substExpr sigma e)
  ToplevelLet x t e cont -> ToplevelLet x (subst' sigma t) (substExpr sigma e) (substToplevelExpr sigma cont)
  ToplevelLetRec f args ret body cont -> ToplevelLetRec f (map (second (subst' sigma)) args) (subst' sigma ret) (substExpr sigma body) (substToplevelExpr sigma cont)

substProgram :: Subst -> Program -> Program
substProgram = substToplevelExpr

-- | `run` does type inference.
-- This assumes that program has no name conflicts.
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.TypeInfer" $ do
  eqns <- formularizeProgram prog
  let (eqns', assertions) = sortEquations eqns
  let eqns'' = mergeAssertions assertions
  sigma <- solveEquations (eqns' ++ eqns'')
  prog <- return $ substProgram sigma prog
  typecheckProgram prog
  return prog
