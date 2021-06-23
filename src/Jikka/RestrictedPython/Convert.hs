{-# LANGUAGE FlexibleContexts #-}

module Jikka.RestrictedPython.Convert
  ( run,
    run',
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Language.Expr as Y
import qualified Jikka.RestrictedPython.Convert.Alpha as Alpha
import qualified Jikka.RestrictedPython.Convert.RemoveUnbalancedIf as RemoveUnbalancedIf
import qualified Jikka.RestrictedPython.Convert.RemoveUnreachable as RemoveUnreachable
import qualified Jikka.RestrictedPython.Convert.ResolveBuiltin as ResolveBuiltin
import qualified Jikka.RestrictedPython.Convert.SplitLoops as SplitLoops
import qualified Jikka.RestrictedPython.Convert.ToCore as ToCore
import qualified Jikka.RestrictedPython.Convert.TypeInfer as TypeInfer
import qualified Jikka.RestrictedPython.Language.Expr as X

run' :: (MonadAlpha m, MonadError Error m) => X.Program -> m X.Program
run' prog = do
  prog <- return $ RemoveUnreachable.run prog
  prog <- return $ RemoveUnbalancedIf.run prog
  prog <- ResolveBuiltin.run prog
  prog <- Alpha.run prog
  prog <- TypeInfer.run prog
  SplitLoops.run prog

run :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
run prog = do
  prog <- run' prog
  ToCore.run prog
