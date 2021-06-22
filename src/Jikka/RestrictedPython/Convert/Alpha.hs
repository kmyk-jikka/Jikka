{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Convert.Alpha
  ( run,
  )
where

import Control.Monad.State.Strict
import Data.List (delete, intersect)
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

withToplevelScope :: MonadState Env m => m a -> m a
withToplevelScope f = do
  env <- get
  x <- f
  put env
  return x

withScope :: MonadState Env m => m a -> m a
withScope f = do
  modify' $ \env ->
    env
      { currentMapping = [],
        parentMappings = currentMapping env : parentMappings env
      }
  x <- f
  modify' $ \env ->
    env
      { currentMapping = head (parentMappings env),
        parentMappings = tail (parentMappings env)
      }
  return x

-- | `renameNew` renames given variables and record them to the `Env`.
renameNew :: (MonadAlpha m, MonadState Env m) => VarName -> m VarName
renameNew x = do
  env <- get
  case lookupName x (env {currentMapping = []}) of
    Just y -> return y
    Nothing -> do
      y <- genVarName x
      when (unVarName x /= "_") $ do
        put $
          env
            { currentMapping = (x, y) : currentMapping env
            }
      return y

-- | `renameCompletelyNew` throws errors when given variables already exists in environments.
renameCompletelyNew :: (MonadAlpha m, MonadState Env m, MonadError Error m) => VarName -> m VarName
renameCompletelyNew x = do
  env <- get
  case lookupName x env of
    Just _ -> throwSemanticError $ "cannot redefine variable: " ++ unVarName x
    Nothing -> renameNew x

-- | `renameToplevel` records given variables to the `Env` without actual renaming.
renameToplevel :: (MonadAlpha m, MonadState Env m, MonadError Error m) => VarName -> m VarName
renameToplevel x = do
  env <- get
  case lookupName x env of
    Just _ -> throwSemanticError $ "cannot redefine variable in toplevel: " ++ unVarName x
    Nothing -> do
      when (unVarName x /= "_") $ do
        put $
          env
            { currentMapping = (x, x) : currentMapping env
            }
      return x

popRename :: (MonadState Env m, MonadError Error m) => VarName -> m ()
popRename x =
  when (unVarName x /= "_") $ do
    y <- lookupName' x
    modify' $ \env -> env {currentMapping = delete (x, y) (currentMapping env)}

lookupName :: VarName -> Env -> Maybe VarName
lookupName x env = go (currentMapping env : parentMappings env)
  where
    go [] = Nothing
    go (mapping : mappings) =
      case lookup x mapping of
        Just y -> return y
        Nothing -> go mappings

lookupName' :: (MonadState Env m, MonadError Error m) => VarName -> m VarName
lookupName' x = do
  env <- get
  case lookupName x env of
    Just y -> return y
    Nothing -> throwSymbolError $ "undefined identifier: " ++ unVarName x

-- | `runAnnTarget` renames targets of annotated assignments.
runAnnTarget :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Target -> m Target
runAnnTarget = runTargetGeneric renameNew

-- | `runForTarget` renames targets of for-loops.
runForTarget :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Target -> m Target
runForTarget = runTargetGeneric renameCompletelyNew

-- | `runAugTarget` renames targets of augumented assignments.
runAugTarget :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Target -> m Target
runAugTarget = runTargetGeneric lookupName'

runTargetGeneric :: (MonadState Env m, MonadAlpha m, MonadError Error m) => (VarName -> m VarName) -> Target -> m Target
runTargetGeneric f = \case
  SubscriptTrg f index -> SubscriptTrg <$> runAugTarget f <*> runExpr index
  NameTrg x -> NameTrg <$> f x
  TupleTrg xs -> TupleTrg <$> mapM (runTargetGeneric f) xs

popTarget :: (MonadState Env m, MonadError Error m) => Target -> m ()
popTarget = \case
  SubscriptTrg _ _ -> return ()
  NameTrg x -> popRename x
  TupleTrg xs -> mapM_ popTarget xs

runExpr :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Expr -> m Expr
runExpr = \case
  BoolOp e1 op e2 -> BoolOp <$> runExpr e1 <*> return op <*> runExpr e2
  BinOp e1 op e2 -> BinOp <$> runExpr e1 <*> return op <*> runExpr e2
  UnaryOp op e -> UnaryOp op <$> runExpr e
  Lambda args body ->
    withToplevelScope $ do
      args <- forM args $ \(x, t) -> do
        y <- renameNew x
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
    mapM_ renameNew (w1 `intersect` w2) -- introduce variables to the parent scope
    body1 <- withScope $ do
      runStatements body1
    body2 <- withScope $ do
      runStatements body2
    return $ If e body1 body2
  Assert e -> Assert <$> runExpr e

runStatements :: (MonadState Env m, MonadAlpha m, MonadError Error m) => [Statement] -> m [Statement]
runStatements = mapM runStatement

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
        y <- renameToplevel x
        return (y, t)
      body <- runStatements body
      return $ ToplevelFunctionDef g args ret body
  ToplevelAssert e -> ToplevelAssert <$> runExpr e

runProgram :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = mapM runToplevelStatement

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
