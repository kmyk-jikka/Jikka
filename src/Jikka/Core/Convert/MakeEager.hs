{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.MakeEager
-- Description : convert exprs for eager evaluation.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.MakeEager
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.Beta
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Util

runExpr :: MonadAlpha m => Expr -> m Expr
runExpr = \case
  If' t p a b -> case t of
    FunTy _ _ -> do
      return $ If' t p a b
    _ -> do
      x <- genVarName'
      y <- genVarName'
      return $ App (If' (FunTy UnitTy t) p (Lam x UnitTy a) (Lam y UnitTy b)) (Tuple' [])
  e -> return e

runToplevelExpr :: (MonadAlpha m, MonadError Error m) => ToplevelExpr -> m ToplevelExpr
runToplevelExpr = \case
  ResultExpr e -> ResultExpr <$> runExpr e
  ToplevelLet x t e cont -> ToplevelLet x t <$> runExpr e <*> runToplevelExpr cont
  ToplevelLetRec f args ret body cont -> case args of
    [] -> do
      x <- genVarName'
      let g = App (Var f) (Tuple' [])
      body <- substitute f g body
      cont <- substituteToplevelExpr f g cont
      ToplevelLetRec f [(x, UnitTy)] ret <$> runExpr body <*> runToplevelExpr cont
    args -> ToplevelLetRec f args ret <$> runExpr body <*> runToplevelExpr cont

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = runToplevelExpr

-- | `run` wraps some exprs with lambda redundant things from AST.
-- Specifically, this converts @if p then a else b@ to something like @(if p then (lambda. a) else (lambda. b))()@.
--
-- For example, this converts:
--
-- > let rec fact n =
-- >     if n == 0 then
-- >         1
-- >     else
-- >         n * fact (n - 1)
-- > in fact 10
--
-- to:
--
-- > let rec fact n =
-- >     (if n == 0 then
-- >         fun -> 1
-- >     else
-- >         fun -> n * fact (n - 1)
-- >     )()
-- > in fact 10
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.MakeEager" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
    ensureEagerlyEvaluatable prog
  return prog
