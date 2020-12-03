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

import Control.Monad.Except
import Jikka.Common.Alpha
import Jikka.Common.Language.Name
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint (typecheckProgram')

gensym :: MonadAlpha m => m VarName
gensym = rename' (VarName "") <$> nextCounter

rename :: MonadAlpha m => VarName -> m VarName
rename hint = rename' hint <$> nextCounter

rename' :: VarName -> Int -> VarName
rename' hint i =
  let base = takeWhile (/= '@') (unVarName hint)
   in VarName (base ++ "@" ++ show i)

-- -----------------------------------------------------------------------------
-- run

runExpr :: (MonadAlpha m, MonadError String m) => [(VarName, VarName)] -> Expr -> m Expr
runExpr env = \case
  Var x -> case lookup x env of
    Nothing -> throwError $ "Internal Error: undefined variable: " ++ show x
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

runToplevelExpr :: (MonadAlpha m, MonadError String m) => [(VarName, VarName)] -> ToplevelExpr -> m ToplevelExpr
runToplevelExpr env = \case
  ResultExpr e -> ResultExpr <$> runExpr env e
  ToplevelLet rec f args ret body cont -> do
    g <- rename f
    args <- forM args $ \(x, t) -> do
      y <- rename x
      return (x, y, t)
    let args1 = map (\(x, y, _) -> (x, y)) args
    let args2 = map (\(_, y, t) -> (y, t)) args
    body <- case rec of
      NonRec -> runExpr (args1 ++ env) body
      Rec -> runExpr (args1 ++ (f, g) : env) body
    cont <- runToplevelExpr ((f, g) : env) cont
    return $ ToplevelLet rec g args2 ret body cont

runProgram :: (MonadAlpha m, MonadError String m) => Program -> m Program
runProgram = runToplevelExpr []

run' :: Program -> Int -> Either String (Program, Int)
run' prog i = do
  (prog, i) <- runAlpha i $ runProgram prog
  prog <- typecheckProgram' prog
  return (prog, i)

run :: Program -> Either String Program
run prog = do
  (prog, _) <- runAlpha 0 $ runToplevelExpr [] prog
  typecheckProgram' prog
