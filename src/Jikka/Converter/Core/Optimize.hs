-- |
-- Module      : Jikka.Converter.Core.Optimize
-- Description : is a meta module to combine other optimizers.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.Optimize` is a module to combine other all optimizers.
module Jikka.Converter.Core.Optimize
  ( run,
  )
where

import qualified Jikka.Converter.Core.RemoveUnusedVars as RemoveUnusedVars
import qualified Jikka.Converter.Core.StrengthReduction as StrengthReduction
import Jikka.Language.Core.Expr

run :: Program -> Either String Program
run prog = do
  prog <- RemoveUnusedVars.run prog
  StrengthReduction.run prog
