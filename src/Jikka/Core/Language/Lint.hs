{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Jikka.Core.Language.Lint
-- Description : checks the invariants (e.g. types) of data types of our core language.
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

ensureWellTyped :: MonadError Error m => Program -> m ()
ensureWellTyped prog = wrapError' "Jikka.Core.Language.Lint.ensureWellTyped" $ do
  _ <- typecheckProgram prog
  return ()
