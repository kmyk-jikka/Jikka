{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.Alpha
-- Description : renames variables in exprs.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.Alpha` renames variables in exprs to avoid name conflictions, even if the scopes of two variables are distinct.
module Jikka.Core.Convert.Alpha where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.Expr

gensym :: MonadAlpha m => m VarName
gensym = rename' (VarName "") <$> nextCounter

rename :: MonadAlpha m => VarName -> m VarName
rename hint = rename' hint <$> nextCounter

rename' :: VarName -> Int -> VarName
rename' hint i =
  let base = takeWhile (/= '$') (unVarName hint)
   in VarName (base ++ "$" ++ show i)

-- -----------------------------------------------------------------------------
-- run

runExpr :: (MonadAlpha m, MonadError Error m) => [(VarName, VarName)] -> Expr -> m Expr
runExpr env = \case
  Var x -> case lookup x env of
    Nothing -> throwInternalError $ "undefined variable: " ++ show x
    Just y -> return $ Var y
  Lit lit -> return $ Lit lit
  App f args -> App <$> runExpr env f <*> mapM (runExpr env) args
  Lam args body -> do
    args <- forM args $ \(x, t) -> do
      y <- rename x
      return (x, y, t)
    let args1 = map (\(x, y, _) -> (x, y)) args
    let args2 = map (\(_, y, t) -> (y, t)) args
    body <- runExpr (reverse args1 ++ env) body
    return $ Lam args2 body
  Let x t e1 e2 -> do
    e1 <- runExpr env e1
    y <- rename x
    e2 <- runExpr ((x, y) : env) e2
    return $ Let y t e1 e2

runToplevelExpr :: (MonadAlpha m, MonadError Error m) => [(VarName, VarName)] -> ToplevelExpr -> m ToplevelExpr
runToplevelExpr env = \case
  ResultExpr e -> ResultExpr <$> runExpr env e
  ToplevelLet x t e cont -> do
    y <- rename x
    e <- runExpr env e
    cont <- runToplevelExpr ((x, y) : env) cont
    return $ ToplevelLet y t e cont
  ToplevelLetRec f args ret body cont -> do
    g <- rename f
    args <- forM args $ \(x, t) -> do
      y <- rename x
      return (x, y, t)
    let args1 = map (\(x, y, _) -> (x, y)) args
    let args2 = map (\(_, y, t) -> (y, t)) args
    body <- runExpr (args1 ++ (f, g) : env) body
    cont <- runToplevelExpr ((f, g) : env) cont
    return $ ToplevelLetRec g args2 ret body cont

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = runToplevelExpr []

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.Alpha" $ do
  runToplevelExpr [] prog
