{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Language.Util
  ( -- * generating symbols
    genType,
    genVarName,
    genVarName',

    -- * free variables
    freeTyVars,
    freeVars,
    freeVars',
    freeVarsTarget,
    freeVarsTarget',

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
    mapSubExpr,
    listSubExprs,

    -- * traversing exprs
    mapExprTargetM,
    mapExprStatementM,
    mapExprM,
    listExprs,

    -- * exprs
    hasFunctionCall,
    isSmallExpr,
    dropLocation,

    -- * targets
    targetVars,
    targetVars',
    hasSubscriptTrg,
    hasBareNameTrg,
    exprToTarget,
    targetToExpr,

    -- * programs
    toplevelMainDef,
  )
where

import Control.Monad.Identity
import Control.Monad.Writer.Strict
import Data.List (delete, nub)
import Jikka.Common.Alpha
import Jikka.Common.Location
import Jikka.RestrictedPython.Language.Expr

genType :: MonadAlpha m => m Type
genType = do
  i <- nextCounter
  return $ VarTy (TypeName ('$' : show i))

genVarName :: MonadAlpha m => VarName' -> m VarName'
genVarName x = do
  i <- nextCounter
  let base = if unVarName (value' x) == "_" then "" else takeWhile (/= '$') (unVarName (value' x))
  return $ WithLoc' (loc' x) (VarName (base ++ '$' : show i))

genVarName' :: MonadAlpha m => m VarName'
genVarName' = genVarName (withoutLoc (VarName "_"))

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
      StringTy -> []
      SideEffectTy -> []

-- | `freeVars'` reports all free variables.
freeVars :: Expr' -> [VarName]
freeVars = nub . map value' . freeVars'

-- | `freeVars'` reports all free variables with their locations, i.e. occurrences.
-- For examples, @x + x@ and @x@ have the same free variables @x@ but they have different sets of occurrences of free variable.
freeVars' :: Expr' -> [VarName']
freeVars' (WithLoc' _ e0) = case e0 of
  BoolOp e1 _ e2 -> freeVars' e1 ++ freeVars' e2
  BinOp e1 _ e2 -> freeVars' e1 ++ freeVars' e2
  UnaryOp _ e -> freeVars' e
  Lambda args e -> foldl (\vars (x, _) -> delete x vars) (freeVars' e) args
  IfExp e1 e2 e3 -> freeVars' e1 ++ freeVars' e2 ++ freeVars' e3
  ListComp e (Comprehension x iter pred) -> freeVars' iter ++ foldl (\vars x -> delete x vars) (freeVars' e ++ concatMap freeVars' pred) (targetVars' x)
  Compare e1 _ e2 -> freeVars' e1 ++ freeVars' e2
  Call f args -> concatMap freeVars' (f : args)
  Constant _ -> []
  Attribute e _ -> freeVars' e
  Subscript e1 e2 -> freeVars' e1 ++ freeVars' e2
  Name x -> [x]
  List _ es -> concatMap freeVars' es
  Tuple es -> concatMap freeVars' es
  SubscriptSlice e from to step -> freeVars' e ++ concatMap freeVars' from ++ concatMap freeVars' to ++ concatMap freeVars' step

freeVarsTarget :: Target' -> [VarName]
freeVarsTarget = nub . map value' . freeVarsTarget'

freeVarsTarget' :: Target' -> [VarName']
freeVarsTarget' (WithLoc' _ x) = case x of
  SubscriptTrg _ e -> freeVars' e
  NameTrg _ -> []
  TupleTrg xs -> concatMap freeVarsTarget' xs

doesAlwaysReturn :: Statement -> Bool
doesAlwaysReturn = \case
  Return _ -> True
  AugAssign _ _ _ -> False
  AnnAssign _ _ _ -> False
  For _ _ _ -> False
  If _ body1 body2 -> any doesAlwaysReturn body1 && any doesAlwaysReturn body2
  Assert _ -> False
  Expr' _ -> False

doesPossiblyReturn :: Statement -> Bool
doesPossiblyReturn = \case
  Return _ -> True
  AugAssign _ _ _ -> False
  AnnAssign _ _ _ -> False
  For _ _ body -> any doesPossiblyReturn body
  If _ body1 body2 -> any doesPossiblyReturn body1 || any doesPossiblyReturn body2
  Assert _ -> False
  Expr' _ -> False

-- | `mapSubExprM` replaces all exprs in a given expr using a given function.
-- This may breaks various constraints.
mapSubExprM :: Monad m => (Expr' -> m Expr') -> Expr' -> m Expr'
mapSubExprM f = go
  where
    go e0 =
      f . WithLoc' (loc' e0) =<< case value' e0 of
        BoolOp e1 op e2 -> BoolOp <$> go e1 <*> return op <*> go e2
        BinOp e1 op e2 -> BinOp <$> go e1 <*> return op <*> go e2
        UnaryOp op e -> UnaryOp op <$> go e
        Lambda args body -> Lambda args <$> go body
        IfExp e1 e2 e3 -> IfExp <$> go e1 <*> go e2 <*> go e3
        ListComp e (Comprehension x iter pred) -> do
          e <- go e
          x <- mapExprTargetM f x
          iter <- go iter
          pred <- mapM go pred
          return $ ListComp e (Comprehension x iter pred)
        Compare e1 op e2 -> Compare <$> go e1 <*> return op <*> go e2
        Call g args -> Call <$> go g <*> mapM go args
        Constant const -> return $ Constant const
        Attribute e x -> Attribute <$> go e <*> pure x
        Subscript e1 e2 -> Subscript <$> go e1 <*> go e2
        Name x -> return $ Name x
        List t es -> List t <$> mapM go es
        Tuple es -> Tuple <$> mapM go es
        SubscriptSlice e from to step -> SubscriptSlice <$> go e <*> mapM go from <*> mapM go to <*> mapM go step

mapSubExpr :: (Expr' -> Expr') -> Expr' -> Expr'
mapSubExpr f = runIdentity . mapSubExprM (return . f)

listSubExprs :: Expr' -> [Expr']
listSubExprs = reverse . getDual . execWriter . mapSubExprM go
  where
    go e = do
      tell $ Dual [e]
      return e

mapExprTargetM :: Monad m => (Expr' -> m Expr') -> Target' -> m Target'
mapExprTargetM f x =
  WithLoc' (loc' x) <$> case value' x of
    SubscriptTrg x e -> SubscriptTrg <$> mapExprTargetM f x <*> f e
    NameTrg x -> return $ NameTrg x
    TupleTrg xs -> TupleTrg <$> mapM (mapExprTargetM f) xs

mapExprStatementM :: Monad m => (Expr' -> m Expr') -> Statement -> m Statement
mapExprStatementM f = \case
  Return e -> Return <$> f e
  AugAssign x op e -> AugAssign <$> mapExprTargetM f x <*> pure op <*> f e
  AnnAssign x t e -> AnnAssign <$> mapExprTargetM f x <*> pure t <*> f e
  For x iter body -> For <$> mapExprTargetM f x <*> f iter <*> mapM (mapExprStatementM f) body
  If e body1 body2 -> If <$> f e <*> mapM (mapExprStatementM f) body1 <*> mapM (mapExprStatementM f) body2
  Assert e -> Assert <$> f e
  Expr' e -> Expr' <$> f e

mapExprToplevelStatementM :: Monad m => (Expr' -> m Expr') -> ToplevelStatement -> m ToplevelStatement
mapExprToplevelStatementM f = \case
  ToplevelAnnAssign x t e -> ToplevelAnnAssign x t <$> f e
  ToplevelFunctionDef g args ret body -> ToplevelFunctionDef g args ret <$> mapM (mapExprStatementM f) body
  ToplevelAssert e -> ToplevelAssert <$> f e

mapExprM :: Monad m => (Expr' -> m Expr') -> Program -> m Program
mapExprM f = mapM (mapExprToplevelStatementM f)

listExprs :: Program -> [Expr']
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
  Expr' e -> f $ Expr' e

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

mapLargeStatementM :: Monad m => (Expr' -> [Statement] -> [Statement] -> m [Statement]) -> (Target' -> Expr' -> [Statement] -> m [Statement]) -> Program -> m Program
mapLargeStatementM fIf fFor = mapStatementM go
  where
    go = \case
      Return e -> return [Return e]
      AugAssign x op e -> return [AugAssign x op e]
      AnnAssign x t e -> return [AnnAssign x t e]
      For x iter body -> fFor x iter body
      If e body1 body2 -> fIf e body1 body2
      Assert e -> return [Assert e]
      Expr' e -> return [Expr' e]

mapLargeStatement :: (Expr' -> [Statement] -> [Statement] -> [Statement]) -> (Target' -> Expr' -> [Statement] -> [Statement]) -> Program -> Program
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
          Expr' e -> return [Expr' e]
    body <- concat <$> mapM (mapStatementStatementM go') body
    body <- go body
    return $ ToplevelFunctionDef f args ret body
  ToplevelAssert e -> return $ ToplevelAssert e

mapStatementsM :: Monad m => ([Statement] -> m [Statement]) -> Program -> m Program
mapStatementsM f = mapM (mapStatementsToplevelStatementM f)

mapStatements :: ([Statement] -> [Statement]) -> Program -> Program
mapStatements f = runIdentity . mapStatementsM (return . f)

hasFunctionCall :: Expr' -> Bool
hasFunctionCall = any (check . value') . listSubExprs
  where
    check = \case
      Call _ _ -> True
      _ -> False

-- | `isSmallExpr` is true if the evaluation of a given expr trivially terminates.
isSmallExpr :: Expr' -> Bool
isSmallExpr = not . hasFunctionCall

dropLocation :: Expr' -> Expr'
dropLocation = mapSubExpr go
  where
    go (WithLoc' _ e) = withoutLoc e

targetVars :: Target' -> [VarName]
targetVars = nub . map value' . targetVars'

targetVars' :: Target' -> [VarName']
targetVars' (WithLoc' _ x) = case x of
  SubscriptTrg x _ -> targetVars' x
  NameTrg x -> [x]
  TupleTrg xs -> concatMap targetVars' xs

hasSubscriptTrg :: Target' -> Bool
hasSubscriptTrg (WithLoc' _ x) = case x of
  SubscriptTrg _ _ -> True
  NameTrg _ -> False
  TupleTrg xs -> any hasSubscriptTrg xs

hasBareNameTrg :: Target' -> Bool
hasBareNameTrg (WithLoc' _ x) = case x of
  SubscriptTrg _ _ -> False
  NameTrg _ -> True
  TupleTrg xs -> any hasSubscriptTrg xs

exprToTarget :: Expr' -> Maybe Target'
exprToTarget e =
  WithLoc' (loc' e) <$> case value' e of
    Name x -> Just $ NameTrg x
    Tuple es -> TupleTrg <$> mapM exprToTarget es
    Subscript e1 e2 -> SubscriptTrg <$> exprToTarget e1 <*> pure e2
    _ -> Nothing

targetToExpr :: Target' -> Expr'
targetToExpr e =
  WithLoc' (loc' e) $ case value' e of
    NameTrg x -> Name x
    TupleTrg es -> Tuple (map targetToExpr es)
    SubscriptTrg e1 e2 -> Subscript (targetToExpr e1) e2

toplevelMainDef :: [Statement] -> Program
toplevelMainDef body = [ToplevelFunctionDef (WithLoc' Nothing (VarName "main")) [] IntTy body]
