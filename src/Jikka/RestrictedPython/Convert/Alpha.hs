{-# LANGUAGE FlexibleContexts #-}

module Jikka.RestrictedPython.Convert.Alpha
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Expr

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run = return
