{-# LANGUAGE OverloadedStrings #-}

module Jikka.Main.Subcommand.Execute (run) where

import Control.Monad.Except
import qualified Data.Text.IO as T (readFile)
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse as FromPython
import qualified Jikka.RestrictedPython.Convert.Alpha as Alpha
import qualified Jikka.RestrictedPython.Convert.RemoveUnreachable as RemoveUnreachable
import qualified Jikka.RestrictedPython.Convert.SplitLoops as SplitLoops
import qualified Jikka.RestrictedPython.Convert.TypeInfer as TypeInfer
import qualified Jikka.RestrictedPython.Evaluate as Evaluate
import qualified Jikka.RestrictedPython.Language.Value as Value

run :: FilePath -> ExceptT Error IO ()
run path = flip evalAlphaT 0 $ do
  prog <- liftIO $ T.readFile path
  prog <- liftEither $ FromPython.run path prog
  prog <- ToRestrictedPython.run prog
  prog <- return $ RemoveUnreachable.run prog
  prog <- Alpha.run prog
  prog <- TypeInfer.run prog
  prog <- SplitLoops.run prog
  global <- Evaluate.makeGlobal prog
  entrypoint <- Value.makeEntryPointIO "solve" global
  value <- Evaluate.runWithGlobal global entrypoint
  liftIO $ Value.writeValueIO value
