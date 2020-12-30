{-# LANGUAGE FlexibleContexts #-}

module Jikka.RestrictedPython.Convert.ToCore
  ( run,
  )
where

import Jikka.Common.Error
import qualified Jikka.Core.Language.Expr as Y
import qualified Jikka.RestrictedPython.Language.Expr as X

run :: MonadError Error m => X.Program -> m Y.Program
run = undefined
