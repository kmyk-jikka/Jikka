{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Jikka.Core.Language.Lint
-- Description : verifies various conditions (e.g. well-typed) of exprs of our core language. / core 言語の式の種々の条件 (例: 型付け可能性) を検査します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Core.Language.Lint` module checks the invariants of data types. Mainly, this checks types of `Expr`.
module Jikka.Core.Language.Lint where

import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.TypeCheck

precondition :: MonadError Error m => m a -> m a
precondition = wrapError' "precondition"

postcondition :: MonadError Error m => m a -> m a
postcondition = wrapError' "postcondition"

-- | TODO: implement this
ensureEagerlyEvaluatable :: MonadError Error m => Program -> m ()
ensureEagerlyEvaluatable _ = wrapError' "Jikka.Core.Language.Lint.ensureEagerlyEvaluatable" $ do
  return ()

ensureWellTyped :: MonadError Error m => Program -> m ()
ensureWellTyped prog = wrapError' "Jikka.Core.Language.Lint.ensureWellTyped" $ do
  _ <- typecheckProgram prog
  return ()
