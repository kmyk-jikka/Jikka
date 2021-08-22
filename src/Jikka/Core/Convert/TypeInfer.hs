{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.TypeInfer
-- Description : does type inference. / 型推論を行います。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.TypeInfer
  ( run,
    runExpr,
    runRule,

    -- * internal types and functions
    Equation (..),
    formularizeProgram,
    sortEquations,
    mergeAssertions,
    Subst (..),
    subst,
    substDefault,
    solveEquations,
    substProgram,
  )
where

import Control.Arrow (second)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict (MonadWriter, censor, execWriterT, tell)
import qualified Data.Map.Strict as M
import Data.Monoid (Dual (..))
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Format (formatExpr, formatToplevelExpr, formatType)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.TypeCheck (literalToType, typecheckExpr, typecheckProgram)
import Jikka.Core.Language.Util

data Hint
  = VarHint VarName
  | ExprHint Expr
  | ToplevelExprHint ToplevelExpr
  deriving (Eq, Ord, Show, Read)

data Equation
  = TypeEquation Type Type [Hint]
  | TypeAssertion VarName Type
  deriving (Eq, Ord, Show, Read)

type Eqns = Dual [Equation]

consHint :: Hint -> Equation -> Equation
consHint hint = \case
  TypeEquation t1 t2 hints -> TypeEquation t1 t2 (hint : hints)
  TypeAssertion x t -> TypeAssertion x t

wrapHint :: MonadWriter Eqns m => Hint -> m a -> m a
wrapHint hint = censor (fmap (map (consHint hint)))

wrapErrorFromHint :: MonadError Error m => Hint -> m a -> m a
wrapErrorFromHint = \case
  VarHint x -> wrapError' $ "around variable " ++ unVarName x
  ExprHint e -> wrapError' $ "around expr " ++ summarize (formatExpr e)
  ToplevelExprHint e -> wrapError' $ "around toplevel expr " ++ summarize (formatToplevelExpr e)
  where
    summarize s = case lines s of
      (s : _ : _) -> s ++ " ..."
      _ -> s

wrapErrorFromHints :: MonadError Error m => [Hint] -> m a -> m a
wrapErrorFromHints hints = foldr (\hint f -> wrapErrorFromHint hint . f) id hints

formularizeType :: MonadWriter Eqns m => Type -> Type -> m ()
formularizeType t1 t2 = tell $ Dual [TypeEquation t1 t2 []]

formularizeVarName :: MonadWriter Eqns m => VarName -> Type -> m ()
formularizeVarName x t = tell $ Dual [TypeAssertion x t]

formularizeExpr :: (MonadWriter Eqns m, MonadAlpha m, MonadError Error m) => Expr -> m Type
formularizeExpr e = wrapHint (ExprHint e) $ case e of
  Var x -> do
    t <- genType
    formularizeVarName x t
    return t
  Lit lit -> case lit of
    LitBuiltin (Proj _) [] -> genType -- Proj may have a empty list.
    _ -> literalToType lit
  App f e -> do
    ret <- genType
    t <- formularizeExpr e
    formularizeExpr' f (FunTy t ret)
    return ret
  Lam x t body -> do
    formularizeVarName x t
    ret <- formularizeExpr body
    return $ FunTy t ret
  Let x t e1 e2 -> do
    formularizeVarName x t
    formularizeExpr' e1 t
    formularizeExpr e2
  Assert e1 e2 -> do
    formularizeExpr' e1 BoolTy
    formularizeExpr e2

formularizeExpr' :: (MonadWriter Eqns m, MonadAlpha m, MonadError Error m) => Expr -> Type -> m ()
formularizeExpr' e t = do
  t' <- formularizeExpr e
  wrapHint (ExprHint e) $ do
    formularizeType t t'

formularizeToplevelExpr :: (MonadWriter Eqns m, MonadAlpha m, MonadError Error m) => ToplevelExpr -> m Type
formularizeToplevelExpr e = wrapHint (ToplevelExprHint e) $ case e of
  ResultExpr e -> formularizeExpr e
  ToplevelLet x t e cont -> do
    formularizeVarName x t
    formularizeExpr' e t
    formularizeToplevelExpr cont
  ToplevelLetRec f args ret body cont -> do
    formularizeVarName f (curryFunTy (map snd args) ret)
    mapM_ (uncurry formularizeVarName) args
    formularizeExpr' body ret
    formularizeToplevelExpr cont
  ToplevelAssert e cont -> do
    formularizeExpr' e BoolTy
    formularizeToplevelExpr cont

formularizeProgram :: (MonadAlpha m, MonadError Error m) => Program -> m [Equation]
formularizeProgram prog = getDual <$> execWriterT (formularizeToplevelExpr prog)

sortEquations :: [Equation] -> ([(Type, Type, [Hint])], [(VarName, Type)])
sortEquations = go [] []
  where
    go eqns' assertions [] = (eqns', assertions)
    go eqns' assertions (eqn : eqns) = case eqn of
      TypeEquation t1 t2 hints -> go ((t1, t2, hints) : eqns') assertions eqns
      TypeAssertion x t -> go eqns' ((x, t) : assertions) eqns

mergeAssertions :: [(VarName, Type)] -> [(Type, Type, [Hint])]
mergeAssertions = go M.empty []
  where
    go _ eqns [] = eqns
    go gamma eqns ((x, t) : assertions) = case M.lookup x gamma of
      Nothing -> go (M.insert x t gamma) eqns assertions
      Just t' -> go gamma ((t, t', [VarHint x]) : eqns) assertions

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
  FunTy t ret -> FunTy (subst sigma t) (subst sigma ret)
  DataStructureTy ds -> DataStructureTy ds

unifyTyVar :: (MonadState Subst m, MonadError Error m) => TypeName -> Type -> m ()
unifyTyVar x t =
  if x `elem` freeTyVars t
    then throwInternalError $ "looped type equation " ++ unTypeName x ++ " = " ++ formatType t
    else do
      modify' (Subst . M.insert x t . unSubst) -- This doesn't introduce the loop.

unifyType :: (MonadState Subst m, MonadError Error m) => Type -> Type -> m ()
unifyType t1 t2 = wrapError' ("failed to unify " ++ formatType t1 ++ " and " ++ formatType t2) $ do
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
        else throwInternalError $ "different type ctors " ++ formatType t1 ++ " and " ++ formatType t2
    (FunTy t1 ret1, FunTy t2 ret2) -> do
      unifyType t1 t2
      unifyType ret1 ret2
    _ -> throwInternalError $ "different type ctors " ++ formatType t1 ++ " and " ++ formatType t2

solveEquations :: MonadError Error m => [(Type, Type, [Hint])] -> m Subst
solveEquations eqns = wrapError' "failed to solve type equations" $ do
  (`execStateT` Subst M.empty) $ do
    forM_ eqns $ \(t1, t2, hints) -> do
      wrapErrorFromHints hints $ do
        unifyType t1 t2

-- | `substDefault` replaces all undetermined type variables with the given default type.
substDefault :: Type -> Type -> Type
substDefault t0 = \case
  VarTy _ -> TupleTy []
  IntTy -> IntTy
  BoolTy -> BoolTy
  ListTy t -> ListTy (substDefault t0 t)
  TupleTy ts -> TupleTy (map (substDefault t0) ts)
  FunTy t ret -> FunTy (substDefault t0 t) (substDefault t0 ret)
  DataStructureTy ds -> DataStructureTy ds

subst' :: Maybe Type -> Subst -> Type -> Type
subst' t0 sigma = maybe id substDefault t0 . subst sigma

fixProj :: MonadError Error m => [(VarName, Type)] -> Expr -> m Expr
fixProj env = \case
  Proj' [] i e -> do
    -- fix Proj with a empty list
    t <- typecheckExpr env e
    case t of
      TupleTy ts -> return $ Proj' ts i e
      _ -> throwInternalError $ "type of argument of proj must be a tuple: " ++ formatType t
  e -> return e

substProgram :: MonadError Error m => Maybe Type -> Subst -> Program -> m Program
substProgram t0 sigma = mapExprProgramM (mapSubExprM fixProj) . mapTypeProgram (subst' t0 sigma)

substExpr :: MonadError Error m => Maybe Type -> Subst -> [(VarName, Type)] -> Expr -> m Expr
substExpr t0 sigma env = mapSubExprM fixProj env . mapTypeExpr (subst' t0 sigma)

-- | `run` does type inference.
--
-- * This assumes that program has no name conflicts.
--
-- Before:
--
-- > let f = fun y -> y
-- > in let x = 1
-- > in f(x + x)
--
-- After:
--
-- > let f: int -> int = fun y: int -> y
-- > in let x: int = 1
-- > in f(x + x)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.TypeInfer" $ do
  eqns <- formularizeProgram prog
  let (eqns', assertions) = sortEquations eqns
  let eqns'' = mergeAssertions assertions
  sigma <- solveEquations (eqns' ++ eqns'')
  let t0 = Just UnitTy
  prog <- substProgram t0 sigma prog
  postcondition $ do
    typecheckProgram prog
  return prog

runExpr :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> m Expr
runExpr env e = wrapError' "Jikka.Core.Convert.TypeInfer" $ do
  eqns <- getDual <$> execWriterT (formularizeExpr e)
  let (eqns', assertions) = sortEquations eqns
  let eqns'' = mergeAssertions assertions
  sigma <- solveEquations (eqns' ++ eqns'')
  let t0 = Nothing -- don't use substDefault
  env <- return $ map (second (subst' t0 sigma)) env
  e <- substExpr t0 sigma env e
  postcondition $ do
    typecheckExpr env e
  return e

runRule :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> Expr -> m ([(VarName, Type)], Expr, Expr)
runRule args e1 e2 = wrapError' "Jikka.Core.Convert.TypeInfer" $ do
  eqns <- (getDual <$>) . execWriterT $ do
    t <- formularizeExpr e1
    formularizeExpr' e2 t
  let (eqns', assertions) = sortEquations eqns
  let eqns'' = mergeAssertions assertions
  sigma <- solveEquations (eqns' ++ eqns'')
  args <- return $ map (second (subst sigma)) args -- don't use substDefault
  e1 <- return $ mapTypeExpr (subst sigma) e1 -- don't use substDefault
  e2 <- return $ mapTypeExpr (subst sigma) e2 -- don't use substDefault
  return (args, e1, e2)
