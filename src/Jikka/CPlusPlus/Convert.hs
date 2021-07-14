{-# LANGUAGE FlexibleContexts #-}

module Jikka.CPlusPlus.Convert
  ( run,
  )
where

import qualified Jikka.CPlusPlus.Convert.AddMain as AddMain
import qualified Jikka.CPlusPlus.Convert.FromCore as FromCore
import qualified Jikka.CPlusPlus.Language.Expr as Y
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.IOFormat
import qualified Jikka.Core.Language.Expr as X

run :: (MonadAlpha m, MonadError Error m) => X.Program -> IOFormat -> m Y.Program
run prog format = do
  prog <- FromCore.run prog
  AddMain.run prog format
