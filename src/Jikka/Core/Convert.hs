{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Jikka.Core.Convert
-- Description : is a module to combine other optimizers. / 他の最適化器を組み合わせて実行する module です。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Core.Convert` is a module to combine other all optimizers.
module Jikka.Core.Convert
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.Alpha as Alpha
import qualified Jikka.Core.Convert.ConstantFolding as ConstantFolding
import qualified Jikka.Core.Convert.ConstantPropagation as ConstantPropagation
import qualified Jikka.Core.Convert.ImmediateAppToLet as ImmediateAppToLet
import qualified Jikka.Core.Convert.LinearFunction as LinearFunction
import qualified Jikka.Core.Convert.PropagateMod as PropagateMod
import qualified Jikka.Core.Convert.RemoveUnusedVars as RemoveUnusedVars
import qualified Jikka.Core.Convert.StrengthReduction as StrengthReduction
import qualified Jikka.Core.Convert.TrivialLetElimination as TrivialLetElimination
import qualified Jikka.Core.Convert.TypeInfer as TypeInfer
import qualified Jikka.Core.Convert.UnpackTuple as UnpackTuple
import Jikka.Core.Language.Expr

run' :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run' prog = do
  prog <- Alpha.run prog
  prog <- TypeInfer.run prog
  prog <- RemoveUnusedVars.run prog
  prog <- ImmediateAppToLet.run prog
  prog <- TrivialLetElimination.run prog
  prog <- UnpackTuple.run prog
  prog <- LinearFunction.run prog
  prog <- PropagateMod.run prog
  prog <- ConstantPropagation.run prog
  prog <- ConstantFolding.run prog
  StrengthReduction.run prog

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog =
  let iteration = 20
   in foldM (\prog _ -> run' prog) prog [0 .. iteration - 1]
