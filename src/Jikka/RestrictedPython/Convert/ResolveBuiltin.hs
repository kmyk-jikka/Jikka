{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

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

runExpr :: (MonadAlpha m, MonadError Error m) => Expr -> m Expr
runExpr = mapSubExprM go
  where
    go = \case
      Name x -> resolveUniqueBuiltin x
      Call (Name f) args -> Call <$> resolveBuiltin f (length args) <*> pure args
      e -> return e

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
