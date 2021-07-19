{-# LANGUAGE FlexibleContexts #-}

module Jikka.CPlusPlus.Convert
  ( run,
  )
where

import qualified Jikka.CPlusPlus.Convert.AddMain as AddMain
import qualified Jikka.CPlusPlus.Convert.FromCore as FromCore
import qualified Jikka.CPlusPlus.Convert.MoveSemantics as MoveSemantics
import qualified Jikka.CPlusPlus.Convert.OptimizeRange as OptimizeRange
import qualified Jikka.CPlusPlus.Convert.UnpackTuples as UnpackTuples
import qualified Jikka.CPlusPlus.Language.Expr as Y
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.IOFormat
import qualified Jikka.Core.Language.Expr as X

run :: (MonadAlpha m, MonadError Error m) => X.Program -> IOFormat -> m Y.Program
run prog format = do
  prog <- FromCore.run prog
  let go prog = do
        prog <- UnpackTuples.run prog
        prog <- MoveSemantics.run prog
        OptimizeRange.run prog
  prog <- go prog
  prog <- go prog
  prog <- go prog
  AddMain.run prog format
