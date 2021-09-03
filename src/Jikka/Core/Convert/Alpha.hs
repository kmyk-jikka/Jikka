{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.Alpha
-- Description : does alpha-conversion. / alpha 変換をします。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.Alpha
  ( run,
    runProgram,
    runToplevelExpr,
    runExpr,
  )
where

import Control.Monad.State.Strict
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint

type UsedVars = [VarName]

type RenameMapping = [(VarName, VarName)]

rename :: (MonadState UsedVars m, MonadAlpha m) => VarName -> m VarName
rename x = do
  used <- get
  y <-
    if x `notElem` used
      then return x
      else do
        let base = takeWhile (/= '$') (unVarName x)
        i <- nextCounter
        return $ VarName (base ++ "$" ++ show i)
  put $ y : used
  return y

runExpr' :: (MonadState UsedVars m, MonadAlpha m, MonadError Error m) => RenameMapping -> Expr -> m Expr
runExpr' env = \case
  Var x -> case lookup x env of
    Nothing -> throwInternalError $ "undefined variable: " ++ unVarName x
    Just y -> return $ Var y
  Lit lit -> return $ Lit lit
  App f e -> App <$> runExpr' env f <*> runExpr' env e
  Lam x t body -> do
    y <- rename x
    body <- runExpr' ((x, y) : env) body
    return $ Lam y t body
  Let x t e1 e2 -> do
    e1 <- runExpr' env e1
    y <- rename x
    e2 <- runExpr' ((x, y) : env) e2
    return $ Let y t e1 e2
  Assert e1 e2 -> Assert <$> runExpr' env e1 <*> runExpr' env e2

runExpr :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> m Expr
runExpr env e = wrapError' "Jikka.Core.Convert.Alpha.runExpr" $ do
  evalStateT (runExpr' (map (\(x, _) -> (x, x)) env) e) (map fst env)

runToplevelExpr' :: (MonadState UsedVars m, MonadAlpha m, MonadError Error m) => RenameMapping -> ToplevelExpr -> m ToplevelExpr
runToplevelExpr' env = \case
  ResultExpr e -> ResultExpr <$> runExpr' env e
  ToplevelLet x t e cont -> do
    y <- rename x
    e <- runExpr' env e
    cont <- runToplevelExpr' ((x, y) : env) cont
    return $ ToplevelLet y t e cont
  ToplevelLetRec f args ret body cont -> do
    g <- rename f
    args <- forM args $ \(x, t) -> do
      y <- rename x
      return (x, y, t)
    let args1 = map (\(x, y, _) -> (x, y)) args
    let args2 = map (\(_, y, t) -> (y, t)) args
    body <- runExpr' (args1 ++ (f, g) : env) body
    cont <- runToplevelExpr' ((f, g) : env) cont
    return $ ToplevelLetRec g args2 ret body cont
  ToplevelAssert e1 e2 -> ToplevelAssert <$> runExpr' env e1 <*> runToplevelExpr' env e2

runToplevelExpr :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> ToplevelExpr -> m ToplevelExpr
runToplevelExpr env e = wrapError' "Jikka.Core.Convert.Alpha.runToplevelExpr" $ do
  evalStateT (runToplevelExpr' (map (\(x, _) -> (x, x)) env) e) (map fst env)

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram prog = wrapError' "Jikka.Core.Convert.Alpha.runProgram" $ do
  prog <- evalStateT (runToplevelExpr' [] prog) []
  postcondition $ do
    ensureAlphaConverted prog
  return prog

-- | `run` renames variables in exprs to avoid name conflictions, even if the scopes of two variables are distinct.
--
-- == Examples
--
-- Before:
--
-- > let x = 0
-- > in y = x + x
-- > in x = x + y
-- > x + y
--
-- After:
--
-- > let x0 = 0
-- > in y1 = x0 + x0
-- > in x2 = x0 + y1
-- > x2 + y1
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run = runProgram
