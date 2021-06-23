{-# LANGUAGE FlexibleContexts #-}

module Jikka.CPlusPlus.Convert
  ( run,
  )
where

import qualified Jikka.CPlusPlus.Convert.FromCore as FromCore
import qualified Jikka.CPlusPlus.Language.Expr as Y
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.ANormal as ANormal
import qualified Jikka.Core.Language.Expr as X

run :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
run prog = do
  prog <- ANormal.run prog
  FromCore.run prog
