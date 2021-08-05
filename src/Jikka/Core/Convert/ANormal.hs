{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.ANormal
-- Description : converts exprs to A-normal form. / 式を A 正規形に変換します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.ANormal
  ( run,
  )
where

import Jikka.Common.Alpha (MonadAlpha)
import Jikka.Common.Error
import qualified Jikka.Core.Convert.Alpha as Alpha (runProgram)
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.TypeCheck
import Jikka.Core.Language.Util

destruct :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> m ([(VarName, Type)], Expr -> Expr, Expr)
destruct env = \case
  e@Var {} -> return (env, id, e)
  e@Lit {} -> return (env, id, e)
  e@App {} -> do
    x <- genVarName'
    t <- typecheckExpr env e
    return ((x, t) : env, Let x t e, Var x)
  e@Lam {} -> do
    x <- genVarName'
    t <- typecheckExpr env e
    return ((x, t) : env, Let x t e, Var x)
  Let x t e1 e2 -> do
    (env, ctx, e1) <- destruct env e1
    (env, ctx', e2) <- destruct ((x, t) : env) e2
    return (env, ctx . Let x t e1 . ctx', e2)
  Assert e1 e2 -> do
    (env, ctx, e1) <- destruct env e1
    (env, ctx', e2) <- destruct env e2
    return (env, ctx . Assert e1 . ctx', e2)

runApp :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> [Expr] -> m Expr
runApp env f args = go env id args
  where
    go :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> ([Expr] -> [Expr]) -> [Expr] -> m Expr
    go env acc [] = do
      (_, ctx, f) <- destruct env f
      return $ ctx (uncurryApp f (acc []))
    go env acc (arg : args) = do
      (env, ctx, arg) <- destruct env arg
      e <- go env (acc . (arg :)) args
      return $ ctx e

runExpr :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> m Expr
runExpr env = \case
  Var x -> return $ Var x
  Lit lit -> return $ Lit lit
  e@(App _ _) -> do
    let (f, args) = curryApp e
    f <- runExpr env f
    args <- mapM (runExpr env) args
    case (f, args) of
      (Lit (LitBuiltin If _), [e1, e2, e3]) -> do
        (_, ctx, e1) <- destruct env e1
        return $ ctx (App3 f e1 e2 e3)
      _ -> runApp env f args
  Lam x t body -> Lam x t <$> runExpr ((x, t) : env) body
  Let x t e1 e2 -> do
    e1 <- runExpr env e1
    (env, ctx, e1) <- destruct env e1
    e2 <- runExpr ((x, t) : env) e2
    return $ ctx (Let x t e1 e2)
  Assert e1 e2 -> do
    e1 <- runExpr env e1
    (env, ctx, e1) <- destruct env e1
    e2 <- runExpr env e2
    return $ ctx (Assert e1 e2)

-- | TODO: convert `ToplevelExpr` too
runProgram :: (MonadAlpha m, MonadError Error m) => ToplevelExpr -> m ToplevelExpr
runProgram = mapToplevelExprProgramM go
  where
    go env = \case
      ResultExpr e -> ResultExpr <$> runExpr env e
      ToplevelLet x t e cont -> ToplevelLet x t <$> runExpr env e <*> pure cont
      ToplevelLetRec f args ret body cont -> do
        let t = curryFunTy (map snd args) ret
        let env' = reverse args ++ (f, t) : env
        ToplevelLetRec f args ret <$> runExpr env' body <*> pure cont
      ToplevelAssert e cont -> ToplevelAssert <$> runExpr env e <*> pure cont

-- | `run` makes a given program A-normal form.
-- A program is an A-normal form iff assigned exprs of all let-statements are values or function applications.
-- For example, this converts the following:
--
-- > (let x = 1 in x) + ((fun y -> y) 1)
--
-- to:
--
-- > let x = 1
-- > in let f = fun y -> y
-- > in let z = f x
-- > in z
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.ANormal" $ do
  prog <- Alpha.runProgram prog
  prog <- runProgram prog
  ensureWellTyped prog
  return prog
