{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Converter.Core.Alpha
-- Description : renames variables in exprs.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.Alpha` renames variables in exprs to avoid name conflictions, even if the scopes of two variables are distinct.
module Jikka.Converter.Core.Alpha
  ( run,
    run',
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict
import Jikka.Language.Common.Name
import Jikka.Language.Core.Expr
import Jikka.Language.Core.Lint (typecheckProgram')

-- -----------------------------------------------------------------------------
-- monad

newtype Env
  = Env
      { counter :: Int
      }

type Alpha = StateT Env (Either String)

runAlpha :: Int -> Alpha a -> Either String (a, Int)
runAlpha i a = do
  let env = Env i
  (a, Env i) <- runStateT a env
  return (a, i)

gensym :: VarName -> Alpha VarName
gensym hint = do
  i <- gets counter
  modify $ \env -> env {counter = counter env + 1}
  let base = takeWhile (/= '@') (unVarName hint)
  return $ VarName (base ++ "@" ++ show i)

-- -----------------------------------------------------------------------------
-- run

runExpr :: [(VarName, VarName)] -> Expr -> Alpha Expr
runExpr env = \case
  Var x -> case lookup x env of
    Nothing -> throwError $ "Internal Error: undefined variable: " ++ show x
    Just y -> return $ Var y
  Lit lit -> return $ Lit lit
  App f args -> App <$> runExpr env f <*> mapM (runExpr env) args
  Lam args body -> do
    args <- forM args $ \(x, t) -> do
      y <- gensym x
      return (x, y, t)
    let args1 = map (\(x, y, _) -> (x, y)) args
    let args2 = map (\(_, y, t) -> (y, t)) args
    body <- runExpr (reverse args1 ++ env) body
    return $ Lam args2 body
  Let x t e1 e2 -> do
    e1 <- runExpr env e1
    y <- gensym x
    e2 <- runExpr ((x, y) : env) e2
    return $ Let y t e1 e2

runToplevelExpr :: [(VarName, VarName)] -> ToplevelExpr -> Alpha ToplevelExpr
runToplevelExpr env = \case
  ResultExpr e -> ResultExpr <$> runExpr env e
  ToplevelLet rec f args ret body cont -> do
    g <- gensym f
    args <- forM args $ \(x, t) -> do
      y <- gensym x
      return (x, y, t)
    let args1 = map (\(x, y, _) -> (x, y)) args
    let args2 = map (\(_, y, t) -> (y, t)) args
    body <- case rec of
      NonRec -> runExpr (args1 ++ (f, g) : env) body
      Rec -> runExpr (args1 ++ env) body
    cont <- runToplevelExpr ((f, g) : env) cont
    return $ ToplevelLet rec g args2 ret body cont

run' :: Program -> Int -> Either String (Program, Int)
run' prog i = do
  (prog, i) <- runAlpha i $ runToplevelExpr [] prog
  prog <- typecheckProgram' prog
  return (prog, i)

run :: Program -> Either String Program
run prog = do
  (prog, _) <- runAlpha 0 $ runToplevelExpr [] prog
  typecheckProgram' prog
