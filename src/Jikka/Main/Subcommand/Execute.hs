{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Jikka.Main.Subcommand.Execute (run) where

import Control.Monad.Except
import qualified Data.Text.IO as T (readFile)
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert as ConvertCore
import qualified Jikka.Core.Evaluate as EvaluateCore
import qualified Jikka.Core.Language.Value as ValueCore
import Jikka.Main.Target
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse as FromPython
import qualified Jikka.RestrictedPython.Convert as ToCore
import qualified Jikka.RestrictedPython.Evaluate as EvaluateRestrictedPython
import qualified Jikka.RestrictedPython.Language.Value as ValueRestrictedPythong

runPython :: FilePath -> ExceptT Error IO ()
runPython _ = throwCommandLineError "cannot execute Python"

runRestrictedPython :: FilePath -> ExceptT Error IO ()
runRestrictedPython path = flip evalAlphaT 0 $ do
  prog <- liftIO $ T.readFile path
  prog <- liftEither $ FromPython.run path prog
  prog <- ToRestrictedPython.run prog
  prog <- ToCore.run' prog
  global <- EvaluateRestrictedPython.makeGlobal prog
  entrypoint <- ValueRestrictedPythong.makeEntryPointIO "solve" global
  value <- EvaluateRestrictedPython.runWithGlobal global entrypoint
  liftIO $ ValueRestrictedPythong.writeValueIO value

runCore :: FilePath -> ExceptT Error IO ()
runCore path = flip evalAlphaT 0 $ do
  prog <- liftIO $ T.readFile path
  prog <- liftEither $ FromPython.run path prog
  prog <- ToRestrictedPython.run prog
  prog <- ToCore.run prog
  prog <- ConvertCore.run prog
  value <- EvaluateCore.run prog
  liftIO $ putStrLn (ValueCore.formatValue value)

runCPlusPlus :: FilePath -> ExceptT Error IO ()
runCPlusPlus _ = throwCommandLineError "cannot execute C++"

run :: Target -> FilePath -> ExceptT Error IO ()
run = \case
  PythonTarget -> runPython
  RestrictedPythonTarget -> runRestrictedPython
  CoreTarget -> runCore
  CPlusPlusTarget -> runCPlusPlus
