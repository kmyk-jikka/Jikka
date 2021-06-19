{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Convert.Alpha
  ( run,
  )
where

import Control.Monad.State.Strict
import Data.List (delete)
import qualified Data.Set as S
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Stdlib

data Env = Env
  { currentMapping :: [(VarName, VarName)],
    parentMappings :: [[(VarName, VarName)]]
  }
  deriving (Eq, Ord, Read, Show)

initialEnv :: Env
initialEnv =
  Env
    { currentMapping = [],
      parentMappings = [map (\x -> (x, x)) (S.toList builtinFunctions)]
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

genVarName :: MonadAlpha m => VarName -> m VarName
genVarName x = do
  i <- nextCounter
  let base = if unVarName x == "_" then "" else takeWhile (/= '$') (unVarName x)
  return $ VarName (base ++ '$' : show i)

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
  Lambda args body -> do
    savedEnv <- get
    args <- forM args $ \(x, t) -> do
      y <- renameNew x
      return (y, t)
    body <- runExpr body
    put savedEnv
    return $ Lambda args body
  IfExp e1 e2 e3 -> do
    e1 <- runExpr e1
    e2 <- runExpr e2
    e3 <- runExpr e3
    return $ IfExp e1 e2 e3
  ListComp e (Comprehension x iter ifs) -> do
    iter <- runExpr iter
    y <- runAnnTarget x
    ifs <- mapM runExpr ifs
    e <- runExpr e
    popTarget x
    return $ ListComp e (Comprehension y iter ifs)
  Compare e1 op e2 -> Compare <$> runExpr e1 <*> return op <*> runExpr e2
  Call f args -> do
    f <- runExpr f
    args <- mapM runExpr args
    return $ Call f args
  Constant const -> return $ Constant const
  Subscript e1 e2 -> Subscript <$> runExpr e1 <*> runExpr e2
  Name x -> Name <$> lookupName' x
  List t es -> List t <$> mapM runExpr es
  Tuple es -> Tuple <$> mapM runExpr es
  SubscriptSlice e from to step -> do
    e <- runExpr e
    from <- mapM runExpr from
    to <- mapM runExpr to
    step <- mapM runExpr step
    return $ SubscriptSlice e from to step

runStatement :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Statement -> m Statement
runStatement = \case
  Return e -> do
    e <- runExpr e
    return $ Return e
  AugAssign x op e -> do
    e <- runExpr e
    x <- runAugTarget x
    return $ AugAssign x op e
  AnnAssign x t e -> do
    e <- runExpr e
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
    body1 <- withScope $ do
      runStatements body1
    body2 <- withScope $ do
      runStatements body2
    return $ If e body1 body2
  Assert e -> do
    e <- runExpr e
    return $ Assert e

runStatements :: (MonadState Env m, MonadAlpha m, MonadError Error m) => [Statement] -> m [Statement]
runStatements = mapM runStatement

runToplevelStatement :: (MonadState Env m, MonadAlpha m, MonadError Error m) => ToplevelStatement -> m ToplevelStatement
runToplevelStatement = \case
  ToplevelAnnAssign x t e -> do
    y <- renameToplevel x
    e <- runExpr e
    return $ ToplevelAnnAssign y t e
  ToplevelFunctionDef f args ret body -> do
    g <- renameToplevel f
    withToplevelScope $ do
      args <- forM args $ \(x, t) -> do
        y <- renameToplevel x
        return (y, t)
      body <- runStatements body
      return $ ToplevelFunctionDef g args ret body
  ToplevelAssert e -> do
    e <- runExpr e
    return $ ToplevelAssert e

runProgram :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = mapM runToplevelStatement

-- | `run` renames variables.
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
-- * This blames leaks of names from for-statements and if-statements at all.
--   For example, the followings are not allowed.
--
--   > if True:
--   >     a = 0
--   > else:
--   >     a = 1
--   > return a  # error
--
--   > i = 0
--   > for i in range(10):
--   >     pass
--   > return i  # error
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.RestrictedPython.Convert.Alpha" $ do
  evalStateT (runProgram prog) initialEnv
