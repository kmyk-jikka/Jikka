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
import qualified Jikka.Core.Convert.ArithmeticExpr as ArithmeticExpr
import qualified Jikka.Core.Convert.Beta as Beta
import qualified Jikka.Core.Convert.BubbleLet as BubbleLet
import qualified Jikka.Core.Convert.CloseAll as CloseAll
import qualified Jikka.Core.Convert.CloseMin as CloseMin
import qualified Jikka.Core.Convert.CloseSum as CloseSum
import qualified Jikka.Core.Convert.ConstantFolding as ConstantFolding
import qualified Jikka.Core.Convert.ConstantPropagation as ConstantPropagation
import qualified Jikka.Core.Convert.ConvexHullTrick as ConvexHullTrick
import qualified Jikka.Core.Convert.CumulativeSum as CumulativeSum
import qualified Jikka.Core.Convert.EqualitySolving as EqualitySolving
import qualified Jikka.Core.Convert.Eta as Eta
import qualified Jikka.Core.Convert.KubaruToMorau as KubaruToMorau
import qualified Jikka.Core.Convert.MakeScanl as MakeScanl
import qualified Jikka.Core.Convert.MatrixExponentiation as MatrixExponentiation
import qualified Jikka.Core.Convert.PropagateMod as PropagateMod
import qualified Jikka.Core.Convert.RemoveUnusedVars as RemoveUnusedVars
import qualified Jikka.Core.Convert.SegmentTree as SegmentTree
import qualified Jikka.Core.Convert.ShortCutFusion as ShortCutFusion
import qualified Jikka.Core.Convert.SortAbs as SortAbs
import qualified Jikka.Core.Convert.SpecializeFoldl as SpecializeFoldl
import qualified Jikka.Core.Convert.TrivialLetElimination as TrivialLetElimination
import qualified Jikka.Core.Convert.TypeInfer as TypeInfer
import qualified Jikka.Core.Convert.UnpackTuple as UnpackTuple
import Jikka.Core.Language.Expr (Program)

run'' :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run'' prog = do
  prog <- RemoveUnusedVars.run prog
  prog <- UnpackTuple.run prog
  prog <- MatrixExponentiation.run prog
  prog <- SpecializeFoldl.run prog
  prog <- SortAbs.run prog
  prog <- MakeScanl.run prog
  prog <- PropagateMod.run prog
  prog <- ConstantPropagation.run prog
  prog <- ConstantFolding.run prog
  prog <- EqualitySolving.run prog
  prog <- ShortCutFusion.run prog
  prog <- CloseSum.run prog
  prog <- CloseAll.run prog
  prog <- CloseMin.run prog
  prog <- KubaruToMorau.run prog
  prog <- CumulativeSum.run prog
  prog <- SegmentTree.run prog
  prog <- BubbleLet.run prog
  prog <- ArithmeticExpr.run prog
  prog <- ConvexHullTrick.run prog
  Eta.run prog

run' :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run' prog = do
  prog <- Beta.run prog
  prog <- TrivialLetElimination.run prog
  prog <- run'' prog
  prog <- run'' prog
  prog <- run'' prog
  prog <- run'' prog
  run'' prog

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = do
  prog <- Alpha.run prog
  prog <- TypeInfer.run prog
  prog <- run' prog
  prog <- run' prog
  prog <- run' prog
  prog <- run' prog
  run' prog
