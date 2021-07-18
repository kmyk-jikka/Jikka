{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Jikka.RestrictedPython.Convert.TypeInfer
-- Description : resolves names of builtin functions using information of arity. / arity の情報を使いながら組み込み関数を名前解決します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Convert.ResolveBuiltin
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Builtin
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Lint
import Jikka.RestrictedPython.Language.Util

runExpr :: (MonadAlpha m, MonadError Error m) => Expr' -> m Expr'
runExpr = mapSubExprM go
  where
    go :: (MonadAlpha m, MonadError Error m) => Expr' -> m Expr'
    go e = case value' e of
      Name x -> resolveUniqueBuiltin x
      Call (WithLoc' _ (Name f)) args -> WithLoc' (loc' e) <$> (Call <$> resolveBuiltin f (length args) <*> pure args)
      Attribute e' a -> WithLoc' (loc' e) <$> resolveAttribute e' a
      _ -> return e

-- | `run` resolves types of polymorphic builtin functions.
-- This assumes there are no assignments to builtin functions, i.e. `doesntHaveAssignmentToBuiltin`.
--
-- For example, the @max@ of @max(xs)@ has a type \(\mathbf{list}(\alpha) \to \alpha\) but the @max@ of @max(x, y, z)@ has a type \(\alpha \times \alpha \times \alpha \to \alpha\).
-- So this function converts @Var "max"@ to @BuiltinMax1 t@, @BuiltinMax t 2@, @BuiltinMax t 3@, etc..
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.RestrictedPython.Convert.ResolveBuiltin" $ do
  ensureDoesntHaveAssignmentToBuiltin prog
  prog <- mapExprM runExpr prog
  ensureDoesntHaveNonResolvedBuiltin prog
  return prog
