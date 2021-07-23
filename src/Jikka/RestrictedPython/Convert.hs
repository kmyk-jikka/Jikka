{-# LANGUAGE FlexibleContexts #-}

module Jikka.RestrictedPython.Convert
  ( run,
    run',
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.IOFormat
import qualified Jikka.Core.Language.Expr as Y
import qualified Jikka.RestrictedPython.Convert.Alpha as Alpha
import qualified Jikka.RestrictedPython.Convert.DefaultMain as DefaultMain
import qualified Jikka.RestrictedPython.Convert.ParseMain as ParseMain
import qualified Jikka.RestrictedPython.Convert.RemoveUnbalancedIf as RemoveUnbalancedIf
import qualified Jikka.RestrictedPython.Convert.RemoveUnreachable as RemoveUnreachable
import qualified Jikka.RestrictedPython.Convert.ResolveBuiltin as ResolveBuiltin
import qualified Jikka.RestrictedPython.Convert.SplitLoops as SplitLoops
import qualified Jikka.RestrictedPython.Convert.ToCore as ToCore
import qualified Jikka.RestrictedPython.Convert.TypeInfer as TypeInfer
import qualified Jikka.RestrictedPython.Convert.UseAppend as UseAppend
import qualified Jikka.RestrictedPython.Language.Expr as X

run' :: (MonadAlpha m, MonadError Error m) => X.Program -> m (X.Program, IOFormat)
run' prog = do
  prog <- return $ RemoveUnreachable.run prog
  prog <- return $ RemoveUnbalancedIf.run prog
  prog <- UseAppend.run prog
  prog <- ResolveBuiltin.run prog
  prog <- Alpha.run prog
  (format, prog) <- ParseMain.run prog -- Run ParseMain before type inference because main function has different semantics.
  prog <- TypeInfer.run prog
  prog <- SplitLoops.run prog
  format <- maybe (DefaultMain.run prog) return format
  return (prog, format)

run :: (MonadAlpha m, MonadError Error m) => X.Program -> m (Y.Program, IOFormat)
run prog = do
  (prog, format) <- run' prog
  prog <- ToCore.run prog
  return (prog, format)
