{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Converter.Core.ValueApps
-- Description : removes complicated exprs in lambda applications.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.ValueApps` removes complicated exprs in lambda applications.
-- This removes `App`, `Lam` and `Let` from the callee and arguments of `App`, and removes `Let` from the bound value of `Let`.
--
-- TODO: rename to a better name
module Jikka.Converter.Core.ValueApps
  ( run,
  )
where

import Control.Monad.Except
import Jikka.Common.Alpha (MonadAlpha, evalAlpha')
import Jikka.Converter.Core.Alpha (gensym)
import qualified Jikka.Converter.Core.Alpha as Alpha (runProgram)
import Jikka.Language.Common.Name
import Jikka.Language.Core.Expr
import Jikka.Language.Core.Lint (TypeEnv, typecheckExpr, typecheckProgram')

destruct :: (MonadAlpha m, MonadError String m) => TypeEnv -> Expr -> m (TypeEnv, Expr -> Expr, Expr)
destruct env = \case
  e@Var {} -> return (env, id, e)
  e@Lit {} -> return (env, id, e)
  e@App {} -> do
    x <- gensym
    t <- typecheckExpr env e
    return ((x, t) : env, Let x t e, Var x)
  e@Lam {} -> do
    x <- gensym
    t <- typecheckExpr env e
    return ((x, t) : env, Let x t e, Var x)
  Let x t e1 e2 -> do
    (env, ctx, e1) <- destruct env e1
    (env, ctx', e2) <- destruct ((x, t) : env) e2
    return (env, ctx . Let x t e1 . ctx', e2)

runApp :: (MonadAlpha m, MonadError String m) => TypeEnv -> Expr -> [Expr] -> m Expr
runApp env f args = go env id args
  where
    go :: (MonadAlpha m, MonadError String m) => [(VarName, Type)] -> ([Expr] -> [Expr]) -> [Expr] -> m Expr
    go env acc [] = do
      (_, ctx, f) <- destruct env f
      return $ ctx (App f (acc []))
    go env acc (arg : args) = do
      (env, ctx, arg) <- destruct env arg
      e <- go env (acc . (arg :)) args
      return $ ctx e

runExpr :: (MonadAlpha m, MonadError String m) => TypeEnv -> Expr -> m Expr
runExpr env = \case
  Var x -> return $ Var x
  Lit lit -> return $ Lit lit
  App f args -> do
    f <- runExpr env f
    args <- mapM (runExpr env) args
    case (f, args) of
      (Lit (LitBuiltin (If _)), [e1, e2, e3]) -> do
        (_, ctx, e1) <- destruct env e1
        return $ ctx (App f [e1, e2, e3])
      _ -> runApp env f args
  Lam args body -> Lam args <$> runExpr (reverse args ++ env) body
  Let x t e1 e2 -> do
    e1 <- runExpr env e1
    (env, ctx, e1) <- destruct env e1
    e2 <- runExpr ((x, t) : env) e2
    return $ ctx (Let x t e1 e2)

runToplevelExpr :: (MonadAlpha m, MonadError String m) => TypeEnv -> ToplevelExpr -> m ToplevelExpr
runToplevelExpr env = \case
  ResultExpr e -> ResultExpr <$> runExpr env e
  ToplevelLet rec f args ret body cont -> do
    let t = FunTy (map snd args) ret
    body <- case rec of
      NonRec -> runExpr (reverse args ++ env) body
      Rec -> runExpr (reverse args ++ (f, t) : env) body
    cont <- runToplevelExpr ((f, t) : env) cont
    return $ ToplevelLet rec f args ret body cont

run :: Program -> Either String Program
run prog = do
  prog <- evalAlpha' $ do
    prog <- Alpha.runProgram prog
    runToplevelExpr [] prog
  typecheckProgram' prog
