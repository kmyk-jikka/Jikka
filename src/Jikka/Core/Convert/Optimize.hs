{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Jikka.Core.Convert.Optimize
-- Description : is a meta module to combine other optimizers.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.Optimize` is a module to combine other all optimizers.
module Jikka.Core.Convert.Optimize
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.RemoveUnusedVars as RemoveUnusedVars
import qualified Jikka.Core.Convert.StrengthReduction as StrengthReduction
import Jikka.Core.Language.Expr

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = do
  prog <- RemoveUnusedVars.run prog
  StrengthReduction.run prog
