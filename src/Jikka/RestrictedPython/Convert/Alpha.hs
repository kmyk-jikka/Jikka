{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Convert.Alpha
  ( run,
  )
where

import Control.Monad.State.Strict
import qualified Data.Set as S
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Stdlib

type Env = [(VarName, VarName)]

rename :: MonadAlpha m => VarName -> m VarName
rename x = do
  i <- nextCounter
  let base = if unVarName x == "_" then "" else takeWhile (/= '$') (unVarName x)
  return $ VarName (base ++ '$' : show i)

push :: MonadState Env m => VarName -> VarName -> m ()
push x y
  | unVarName x == "_" = return ()
  | otherwise = modify' ((x, y) :)

push' :: MonadState Env m => [(VarName, VarName, a)] -> m ()
push' xys = mapM_ (\(x, y, _) -> push x y) xys

lookup' :: (MonadState Env m, MonadError Error m) => VarName -> m VarName
lookup' x = do
  env <- get
  case lookup x env of
    Just y -> return y
    Nothing -> throwSymbolError $ "undefined identifier: " ++ unVarName x

runTarget :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Target -> m Target
runTarget = \case
  SubscriptTrg f index -> do
    f <- runTarget f
    index <- runExpr index
    return $ SubscriptTrg f index
  NameTrg x -> do
    y <- rename x
    push x y
    return $ NameTrg y
  TupleTrg xs -> do
    let go [] = return []
        go (x : xs) = do
          y <- runTarget x
          ys <- go xs
          return $ y : ys
    ys <- go xs
    return $ TupleTrg ys

runExpr :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Expr -> m Expr
runExpr = \case
  BoolOp e1 op e2 -> BoolOp <$> runExpr e1 <*> return op <*> runExpr e2
  BinOp e1 op e2 -> BinOp <$> runExpr e1 <*> return op <*> runExpr e2
  UnaryOp op e -> UnaryOp op <$> runExpr e
  Lambda args body -> do
    args <- forM args $ \(x, t) -> do
      y <- rename x
      return (x, y, t)
    savedEnv <- get
    push' args
    let args' = map (\(_, y, t) -> (y, t)) args
    body <- runExpr body
    put savedEnv
    return $ Lambda args' body
  IfExp e1 e2 e3 -> do
    e1 <- runExpr e1
    e2 <- runExpr e2
    e3 <- runExpr e3
    return $ IfExp e1 e2 e3
  ListComp e (Comprehension x iter ifs) -> do
    iter <- runExpr iter
    y <- runTarget x
    ifs <- mapM runExpr ifs
    e <- runExpr e
    return $ ListComp e (Comprehension y iter ifs)
  Compare e1 op e2 -> Compare <$> runExpr e1 <*> return op <*> runExpr e2
  Call f args -> do
    f <- runExpr f
    args <- mapM runExpr args
    return $ Call f args
  Constant const -> return $ Constant const
  Subscript e1 e2 -> Subscript <$> runExpr e1 <*> runExpr e2
  Name x -> Name <$> lookup' x
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
    x <- runTarget x
    return $ AugAssign x op e
  AnnAssign x t e -> do
    e <- runExpr e
    x <- runTarget x
    return $ AnnAssign x t e
  For x e body -> do
    y <- runTarget x
    e <- runExpr e
    savedEnv <- get
    body <- runStatements body
    put savedEnv
    return $ For y e body
  If e body1 body2 -> do
    e <- runExpr e
    savedEnv <- get
    body1 <- runStatements body1
    put savedEnv
    body2 <- runStatements body2
    put savedEnv
    return $ If e body1 body2
  Assert e -> do
    e <- runExpr e
    return $ Assert e

runStatements :: (MonadState Env m, MonadAlpha m, MonadError Error m) => [Statement] -> m [Statement]
runStatements = mapM runStatement

runToplevelStatement :: (MonadState Env m, MonadAlpha m, MonadError Error m) => ToplevelStatement -> m ToplevelStatement
runToplevelStatement = \case
  ToplevelAnnAssign x t e -> do
    y <- rename x
    push x y
    e <- runExpr e
    return $ ToplevelAnnAssign y t e
  ToplevelFunctionDef f args ret body -> do
    g <- rename f
    push f g
    args <- forM args $ \(x, t) -> do
      y <- rename x
      return (x, y, t)
    let args' = map (\(_, y, t) -> (y, t)) args
    savedEnv <- get
    push' args
    body <- runStatements body
    put savedEnv
    return $ ToplevelFunctionDef g args' ret body
  ToplevelAssert e -> do
    e <- runExpr e
    return $ ToplevelAssert e

runProgram :: (MonadState Env m, MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = mapM runToplevelStatement

initialEnv :: Env
initialEnv = map (\x -> (x, x)) (S.toList builtinFunctions)

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.RestrictedPython.Convert.Alpha" $ do
  evalStateT (runProgram prog) initialEnv
