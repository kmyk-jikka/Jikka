{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Language.Util
  ( -- * generating symbols
    genType,
    genVarName,

    -- * free variables
    freeTyVars,
    freeVars,
    freeVarsTarget,

    -- * return-statements
    doesAlwaysReturn,
    doesPossiblyReturn,

    -- * traversing statements
    mapStatement,
    mapStatementM,
    mapLargeStatement,
    mapLargeStatementM,
    mapStatements,
    mapStatementsM,
    listStatements,

    -- * traversing sub exprs
    mapSubExprM,
    listSubExprs,

    -- * traversing exprs
    mapExprTargetM,
    mapExprStatementM,
    mapExprM,
    listExprs,

    -- * targets
    targetVars,
    hasSubscriptTrg,
    hasBareNameTrg,

    -- * IO
    readValueIO,
  )
where

import Control.Monad.Identity
import Control.Monad.Writer.Strict
import Data.List (delete, nub)
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.IO
import Jikka.RestrictedPython.Language.Expr

genType :: MonadAlpha m => m Type
genType = do
  i <- nextCounter
  return $ VarTy (TypeName ('$' : show i))

genVarName :: MonadAlpha m => VarName -> m VarName
genVarName x = do
  i <- nextCounter
  let base = if unVarName x == "_" then "" else takeWhile (/= '$') (unVarName x)
  return $ VarName (base ++ '$' : show i)

freeTyVars :: Type -> [TypeName]
freeTyVars = nub . go
  where
    go = \case
      VarTy x -> [x]
      IntTy -> []
      BoolTy -> []
      ListTy t -> go t
      TupleTy ts -> concat $ mapM go ts
      CallableTy ts ret -> concat $ mapM go (ret : ts)

freeVars :: Expr -> [VarName]
freeVars = nub . go
  where
    go = \case
      BoolOp e1 _ e2 -> go e1 ++ go e2
      BinOp e1 _ e2 -> go e1 ++ go e2
      UnaryOp _ e -> go e
      Lambda args e -> foldl (\vars (x, _) -> delete x vars) (go e) args
      IfExp e1 e2 e3 -> go e1 ++ go e2 ++ go e3
      ListComp e (Comprehension x iter pred) -> go iter ++ foldl (\vars x -> delete x vars) (go e ++ concatMap go pred) (targetVars x)
      Compare e1 _ e2 -> go e1 ++ go e2
      Call f args -> concatMap go (f : args)
      Constant _ -> []
      Subscript e1 e2 -> go e1 ++ go e2
      Name x -> [x]
      List _ es -> concatMap go es
      Tuple es -> concatMap go es
      SubscriptSlice e from to step -> go e ++ concatMap go from ++ concatMap go to ++ concatMap go step

freeVarsTarget :: Target -> [VarName]
freeVarsTarget = nub . go
  where
    go = \case
      SubscriptTrg _ e -> freeVars e
      NameTrg _ -> []
      TupleTrg xs -> concatMap go xs

doesAlwaysReturn :: Statement -> Bool
doesAlwaysReturn = \case
  Return _ -> True
  AugAssign _ _ _ -> False
  AnnAssign _ _ _ -> False
  For _ _ _ -> False
  If _ body1 body2 -> any doesAlwaysReturn body1 && any doesAlwaysReturn body2
  Assert _ -> False

doesPossiblyReturn :: Statement -> Bool
doesPossiblyReturn = \case
  Return _ -> True
  AugAssign _ _ _ -> False
  AnnAssign _ _ _ -> False
  For _ _ body -> any doesPossiblyReturn body
  If _ body1 body2 -> any doesPossiblyReturn body1 || any doesPossiblyReturn body2
  Assert _ -> False

-- | `mapSubExprM` replaces all exprs in a given expr using a given function.
-- This may breaks various constraints.
mapSubExprM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
mapSubExprM f = go
  where
    go = \case
      BoolOp e1 op e2 -> f =<< BoolOp <$> go e1 <*> return op <*> go e2
      BinOp e1 op e2 -> f =<< BinOp <$> go e1 <*> return op <*> go e2
      UnaryOp op e -> f . UnaryOp op =<< go e
      Lambda args body -> f . Lambda args =<< go body
      IfExp e1 e2 e3 -> f =<< IfExp <$> go e1 <*> go e2 <*> go e3
      ListComp e (Comprehension x iter pred) -> do
        x <- mapExprTargetM f x
        iter <- go iter
        pred <- mapM go pred
        f $ ListComp e (Comprehension x iter pred)
      Compare e1 op e2 -> f =<< Compare <$> go e1 <*> return op <*> go e2
      Call g args -> f =<< Call <$> go g <*> mapM go args
      Constant const -> f $ Constant const
      Subscript e1 e2 -> f =<< Subscript <$> go e1 <*> go e2
      Name x -> f $ Name x
      List t es -> f . List t =<< mapM go es
      Tuple es -> f . Tuple =<< mapM go es
      SubscriptSlice e from to step -> f =<< SubscriptSlice <$> go e <*> mapM go from <*> mapM go to <*> mapM go step

listSubExprs :: Expr -> [Expr]
listSubExprs = reverse . getDual . execWriter . mapSubExprM go
  where
    go e = do
      tell $ Dual [e]
      return e

mapExprTargetM :: Monad m => (Expr -> m Expr) -> Target -> m Target
mapExprTargetM f = \case
  SubscriptTrg x e -> SubscriptTrg <$> mapExprTargetM f x <*> f e
  NameTrg x -> return $ NameTrg x
  TupleTrg xs -> TupleTrg <$> mapM (mapExprTargetM f) xs

mapExprStatementM :: Monad m => (Expr -> m Expr) -> Statement -> m Statement
mapExprStatementM f = \case
  Return e -> Return <$> f e
  AugAssign x op e -> AugAssign <$> mapExprTargetM f x <*> pure op <*> f e
  AnnAssign x t e -> AnnAssign <$> mapExprTargetM f x <*> pure t <*> f e
  For x iter body -> For <$> mapExprTargetM f x <*> f iter <*> mapM (mapExprStatementM f) body
  If e body1 body2 -> If <$> f e <*> mapM (mapExprStatementM f) body1 <*> mapM (mapExprStatementM f) body2
  Assert e -> Assert <$> f e

mapExprToplevelStatementM :: Monad m => (Expr -> m Expr) -> ToplevelStatement -> m ToplevelStatement
mapExprToplevelStatementM f = \case
  ToplevelAnnAssign x t e -> ToplevelAnnAssign x t <$> f e
  ToplevelFunctionDef g args ret body -> ToplevelFunctionDef g args ret <$> mapM (mapExprStatementM f) body
  ToplevelAssert e -> ToplevelAssert <$> f e

mapExprM :: Monad m => (Expr -> m Expr) -> Program -> m Program
mapExprM f = mapM (mapExprToplevelStatementM f)

listExprs :: Program -> [Expr]
listExprs = reverse . getDual . execWriter . mapExprM go
  where
    go e = do
      tell $ Dual [e]
      return e

mapStatementStatementM :: Monad m => (Statement -> m [Statement]) -> Statement -> m [Statement]
mapStatementStatementM f = \case
  Return e -> f $ Return e
  AugAssign x op e -> f $ AugAssign x op e
  AnnAssign x t e -> f $ AnnAssign x t e
  For x iter body -> do
    body <- concat <$> mapM (mapStatementStatementM f) body
    f $ For x iter body
  If e body1 body2 -> do
    body1 <- concat <$> mapM (mapStatementStatementM f) body1
    body2 <- concat <$> mapM (mapStatementStatementM f) body2
    f $ If e body1 body2
  Assert e -> f $ Assert e

mapStatementToplevelStatementM :: Monad m => (Statement -> m [Statement]) -> ToplevelStatement -> m ToplevelStatement
mapStatementToplevelStatementM go = \case
  ToplevelAnnAssign x t e -> return $ ToplevelAnnAssign x t e
  ToplevelFunctionDef f args ret body -> do
    body <- concat <$> mapM (mapStatementStatementM go) body
    return $ ToplevelFunctionDef f args ret body
  ToplevelAssert e -> return $ ToplevelAssert e

-- | `mapStatementM` replaces all statements in a given program using a given function.
-- This may breaks various constraints.
mapStatementM :: Monad m => (Statement -> m [Statement]) -> Program -> m Program
mapStatementM f = mapM (mapStatementToplevelStatementM f)

mapStatement :: (Statement -> [Statement]) -> Program -> Program
mapStatement f = runIdentity . mapStatementM (return . f)

mapLargeStatementM :: Monad m => (Expr -> [Statement] -> [Statement] -> m [Statement]) -> (Target -> Expr -> [Statement] -> m [Statement]) -> Program -> m Program
mapLargeStatementM fIf fFor = mapStatementM go
  where
    go = \case
      Return e -> return [Return e]
      AugAssign x op e -> return [AugAssign x op e]
      AnnAssign x t e -> return [AnnAssign x t e]
      For x iter body -> fFor x iter body
      If e body1 body2 -> fIf e body1 body2
      Assert e -> return [Assert e]

mapLargeStatement :: (Expr -> [Statement] -> [Statement] -> [Statement]) -> (Target -> Expr -> [Statement] -> [Statement]) -> Program -> Program
mapLargeStatement fIf fFor = runIdentity . mapLargeStatementM fIf' fFor'
  where
    fIf' e body1 body2 = return $ fIf e body1 body2
    fFor' x iter body = return $ fFor x iter body

listStatements :: Program -> [Statement]
listStatements = reverse . getDual . execWriter . mapStatementM go
  where
    go stmt = do
      tell $ Dual [stmt]
      return [stmt]

mapStatementsToplevelStatementM :: Monad m => ([Statement] -> m [Statement]) -> ToplevelStatement -> m ToplevelStatement
mapStatementsToplevelStatementM go = \case
  ToplevelAnnAssign x t e -> return $ ToplevelAnnAssign x t e
  ToplevelFunctionDef f args ret body -> do
    let go' = \case
          Return e -> return [Return e]
          AugAssign x op e -> return [AugAssign x op e]
          AnnAssign x t e -> return [AnnAssign x t e]
          For x iter body -> do
            body <- go body
            return [For x iter body]
          If e body1 body2 -> do
            body1 <- go body1
            body2 <- go body2
            return [If e body1 body2]
          Assert e -> return [Assert e]
    body <- concat <$> mapM (mapStatementStatementM go') body
    body <- go body
    return $ ToplevelFunctionDef f args ret body
  ToplevelAssert e -> return $ ToplevelAssert e

mapStatementsM :: Monad m => ([Statement] -> m [Statement]) -> Program -> m Program
mapStatementsM f = mapM (mapStatementsToplevelStatementM f)

mapStatements :: ([Statement] -> [Statement]) -> Program -> Program
mapStatements f = runIdentity . mapStatementsM (return . f)

targetVars :: Target -> [VarName]
targetVars = nub . go
  where
    go = \case
      SubscriptTrg x _ -> go x
      NameTrg x -> [x]
      TupleTrg xs -> concatMap go xs

hasSubscriptTrg :: Target -> Bool
hasSubscriptTrg = \case
  SubscriptTrg _ _ -> True
  NameTrg _ -> False
  TupleTrg xs -> any hasSubscriptTrg xs

hasBareNameTrg :: Target -> Bool
hasBareNameTrg = \case
  SubscriptTrg _ _ -> False
  NameTrg _ -> True
  TupleTrg xs -> any hasSubscriptTrg xs

readValueIO :: (MonadIO m, MonadError Error m) => Type -> m Expr
readValueIO = \case
  VarTy _ -> throwRuntimeError "cannot read values of type variables"
  IntTy -> do
    n <- read <$> liftIO getWord
    return $ Constant (ConstInt n)
  BoolTy -> do
    p <- read <$> liftIO getWord
    return $ Constant (ConstBool p)
  ListTy t -> do
    n <- read <$> liftIO getWord
    xs <- replicateM n (readValueIO t)
    return $ List t xs
  TupleTy ts -> Tuple <$> mapM readValueIO ts
  CallableTy _ _ -> throwRuntimeError "cannot read functions"
