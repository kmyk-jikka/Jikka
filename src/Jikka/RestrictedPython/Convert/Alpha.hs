{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Convert.Alpha
  ( run,
  )
where

import Control.Monad.State.Strict
import Data.List (delete, intersect)
import Data.Maybe (isNothing)
import qualified Data.Set as S
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Builtin
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Lint
import Jikka.RestrictedPython.Language.Util
import Jikka.RestrictedPython.Language.VariableAnalysis

data Env = Env
  { currentMapping :: [(VarName, VarName)],
    parentMappings :: [[(VarName, VarName)]]
  }
  deriving (Eq, Ord, Read, Show)

initialEnv :: Env
initialEnv =
  Env
    { currentMapping = [],
      parentMappings = [map (\x -> (x, x)) (S.toList builtinNames)]
    }

withToplevelScope :: (MonadError Error m, MonadState Env m) => m a -> m a
withToplevelScope f = do
  env <- get
  x <- catchError' $ withScope f
  put env
  liftEither x

withScope :: (MonadError Error m, MonadState Env m) => m a -> m a
withScope f = do
  modify' $ \env ->
    env
      { currentMapping = [],
        parentMappings = currentMapping env : parentMappings env
      }
  x <- catchError' f
  modify' $ \env ->
    env
      { currentMapping = head (parentMappings env),
        parentMappings = tail (parentMappings env)
      }
  liftEither x

-- | `renameLocalNew` renames given variables and record them to the `Env`.
renameLocalNew :: (MonadAlpha m, MonadState Env m) => VarName' -> m VarName'
renameLocalNew x = do
  env <- get
  case lookupLocalName x (env {currentMapping = []}) of
    Just y -> return y
    Nothing -> do
      y <- genVarName x
      when (unVarName (value' x) /= "_") $ do
        put $
          env
            { currentMapping = (value' x, value' y) : currentMapping env
            }
      return y

-- | `renameShadow` renames given variables ignoring the current `Env` and record them to the `Env`.
renameShadow :: (MonadAlpha m, MonadState Env m) => VarName' -> m VarName'
renameShadow x = do
  env <- get
  y <- genVarName x
  put $
    env
      { currentMapping = (value' x, value' y) : currentMapping env
      }
  return y

-- | `renameLocalCompletelyNew` throws errors when given variables already exists in environments.
renameLocalCompletelyNew :: (MonadAlpha m, MonadState Env m, MonadError Error m) => VarName' -> m VarName'
renameLocalCompletelyNew x = do
  env <- get
  case lookupLocalName x env of
    Just _ -> maybe id wrapAt (loc' x) . throwSemanticError $ "cannot redefine variable: " ++ unVarName (value' x)
    Nothing -> renameLocalNew x

-- | `renameToplevel` records given variables to the `Env` without actual renaming.
renameToplevel :: (MonadAlpha m, MonadState Env m, MonadError Error m) => VarName' -> m VarName'
renameToplevel x = do
  env <- get
  case lookupName x env of
    Just _ -> do
      let msg =
            if value' x `S.member` builtinNames
              then "cannot assign to builtin function: " ++ unVarName (value' x)
              else "cannot redefine variable in toplevel: " ++ unVarName (value' x)
      maybe id wrapAt (loc' x) $ throwSemanticError msg
    Nothing -> do
      when (unVarName (value' x) /= "_") $ do
        put $
          env
            { currentMapping = (value' x, value' x) : currentMapping env
            }
      return x

-- | `renameToplevelArgument` always introduces a new variable.
renameToplevelArgument :: (MonadAlpha m, MonadState Env m, MonadError Error m) => VarName' -> m VarName'
renameToplevelArgument = renameShadow

popRename :: (MonadState Env m, MonadError Error m) => VarName' -> m ()
popRename x =
  when (unVarName (value' x) /= "_") $ do
    y <- lookupName' x
    modify' $ \env -> env {currentMapping = delete (value' x, value' y) (currentMapping env)}

lookupName :: VarName' -> Env -> Maybe VarName'
lookupName x env = lookupNameFromMappings x (currentMapping env : parentMappings env)

lookupLocalName :: VarName' -> Env -> Maybe VarName'
lookupLocalName x env = lookupNameFromMappings x (reverse (drop 2 (reverse (currentMapping env : parentMappings env))))

lookupNameFromMappings :: VarName' -> [[(VarName, VarName)]] -> Maybe VarName'
lookupNameFromMappings _ [] = Nothing
lookupNameFromMappings x (mapping : mappings) =
  case lookup (value' x) mapping of
    Just y -> return $ WithLoc' (loc' x) y
    Nothing -> lookupNameFromMappings x mappings

lookupName' :: (MonadState Env m, MonadError Error m) => VarName' -> m VarName'
lookupName' x = do
  env <- get
  case lookupName x env of
    Just y -> return y
    Nothing -> maybe id wrapAt (loc' x) . throwSymbolError $ "undefined identifier: " ++ unVarName (value' x)

-- | `runAnnTarget` renames targets of annotated assignments.
runAnnTarget :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Target' -> m Target'
runAnnTarget = runTargetGeneric renameLocalNew

-- | `runForTarget` renames targets of for-loops.
runForTarget :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Target' -> m Target'
runForTarget = runTargetGeneric renameLocalCompletelyNew

-- | `runAugTarget` renames targets of augumented assignments.
runAugTarget :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Target' -> m Target'
runAugTarget = runTargetGeneric lookupName'

runTargetGeneric :: (MonadState Env m, MonadAlpha m, MonadError Error m) => (VarName' -> m VarName') -> Target' -> m Target'
runTargetGeneric f x =
  WithLoc' (loc' x) <$> case value' x of
    SubscriptTrg f index -> SubscriptTrg <$> runAugTarget f <*> runExpr index
    NameTrg x -> NameTrg <$> f x
    TupleTrg xs -> TupleTrg <$> mapM (runTargetGeneric f) xs

popTarget :: (MonadState Env m, MonadError Error m) => Target' -> m ()
popTarget (WithLoc' _ x) = case x of
  SubscriptTrg _ _ -> return ()
  NameTrg x -> popRename x
  TupleTrg xs -> mapM_ popTarget xs

runExpr :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Expr' -> m Expr'
runExpr e0 =
  maybe id wrapAt (loc' e0) $
    WithLoc' (loc' e0) <$> case value' e0 of
      BoolOp e1 op e2 -> BoolOp <$> runExpr e1 <*> return op <*> runExpr e2
      BinOp e1 op e2 -> BinOp <$> runExpr e1 <*> return op <*> runExpr e2
      UnaryOp op e -> UnaryOp op <$> runExpr e
      Lambda args body ->
        withToplevelScope $ do
          args <- forM args $ \(x, t) -> do
            y <- renameLocalNew x
            return (y, t)
          body <- runExpr body
          return $ Lambda args body
      IfExp e1 e2 e3 -> IfExp <$> runExpr e1 <*> runExpr e2 <*> runExpr e3
      ListComp e (Comprehension x iter ifs) -> do
        iter <- runExpr iter
        y <- runAnnTarget x
        ifs <- mapM runExpr ifs
        e <- runExpr e
        popTarget x
        return $ ListComp e (Comprehension y iter ifs)
      Compare e1 op e2 -> Compare <$> runExpr e1 <*> return op <*> runExpr e2
      Call f args -> Call <$> runExpr f <*> mapM runExpr args
      Constant const -> return $ Constant const
      Subscript e1 e2 -> Subscript <$> runExpr e1 <*> runExpr e2
      Name x -> Name <$> lookupName' x
      List t es -> List t <$> mapM runExpr es
      Tuple es -> Tuple <$> mapM runExpr es
      SubscriptSlice e from to step -> SubscriptSlice <$> runExpr e <*> mapM runExpr from <*> mapM runExpr to <*> mapM runExpr step

runStatement :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Statement -> m Statement
runStatement = \case
  Return e -> Return <$> runExpr e
  AugAssign x op e -> do
    e <- runExpr e
    x <- runAugTarget x
    return $ AugAssign x op e
  AnnAssign x t e -> do
    e <- runExpr e -- visit e before x
    x <- runAnnTarget x
    return $ AnnAssign x t e
  For x e body -> do
    e <- runExpr e
    withScope $ do
      y <- runForTarget x
      body <- runStatements body
      return $ For y e body
  If e body1 body2 -> do
    e <- runExpr e
    let (_, WriteList w1) = analyzeStatementsMin body1
    let (_, WriteList w2) = analyzeStatementsMin body2
    forM_ (w1 `intersect` w2) $ \x -> do
      isLocallyUndefined <- isNothing . lookupLocalName (withoutLoc x) <$> get
      when isLocallyUndefined $ do
        renameLocalNew (withoutLoc x) -- introduce variables to the parent scope
        return ()
    body1 <- withScope $ do
      runStatements body1
    body2 <- withScope $ do
      runStatements body2
    return $ If e body1 body2
  Assert e -> Assert <$> runExpr e

runStatements :: (MonadState Env m, MonadAlpha m, MonadError Error m) => [Statement] -> m [Statement]
runStatements stmts = reportErrors =<< mapM (catchError' . runStatement) stmts

runToplevelStatement :: (MonadState Env m, MonadAlpha m, MonadError Error m) => ToplevelStatement -> m ToplevelStatement
runToplevelStatement = \case
  ToplevelAnnAssign x t e -> do
    e <- runExpr e -- visit e before x
    y <- renameToplevel x
    return $ ToplevelAnnAssign y t e
  ToplevelFunctionDef f args ret body -> do
    g <- renameToplevel f
    withToplevelScope $ do
      args <- forM args $ \(x, t) -> do
        y <- renameToplevelArgument x
        return (y, t)
      body <- runStatements body
      return $ ToplevelFunctionDef g args ret body
  ToplevelAssert e -> ToplevelAssert <$> runExpr e

runProgram :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram prog = reportErrors =<< mapM (catchError' . runToplevelStatement) prog

-- | `run` renames variables.
-- This assumes `doesntHaveAssignmentToBuiltin`.
--
-- * This introduce a new name for each assignment if possible.
--   For example, the following
--
--   > x = 21
--   > x += x
--   > x = 42
--   > x += x
--   > for _ in range(100):
--   >     x = x + 1
--   > x = x + 1
--
--   turns the following
--
--   > x0 = 21
--   > x1 += x0
--   > x2 = 42
--   > x3 += x2
--   > for a4 in range(100):
--   >     x3 = x3 + 1
--   > x5 = x3 + 1
--
-- * This blames leaks of loop counters of for-statements, i.e. `doesntHaveLeakOfLoopCounters`.
--   For example, the followings is not allowed.
--
--   > for i in range(10):
--   >     a = 0
--   > return a  # error
--
-- * This blames leaks of names from for-statements and if-statements at all.
--   For example, the followings are not allowed.
--
--   > if True:
--   >     a = 0
--   > else:
--   >     b = 1
--   > return a  # error
--
--   > for i in range(10):
--   >     a = 0
--   > return a  # error
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.RestrictedPython.Convert.Alpha" $ do
  ensureDoesntHaveLeakOfLoopCounters prog
  ensureDoesntHaveAssignmentToBuiltin prog
  evalStateT (runProgram prog) initialEnv
