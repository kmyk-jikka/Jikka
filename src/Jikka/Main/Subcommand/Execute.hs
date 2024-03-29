{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Subcommand.Execute
-- Description : is the entry point of @execute@ subcommand. / @execute@ サブコマンドのエントリポイントです。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
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
  (prog, format) <- ToCore.run' prog
  (args, env) <- ValueRestrictedPythong.readValueIO format
  result <- EvaluateRestrictedPython.run prog args
  ValueRestrictedPythong.writeValueIO format env result

runCore :: FilePath -> ExceptT Error IO ()
runCore path = flip evalAlphaT 0 $ do
  prog <- liftIO $ T.readFile path
  prog <- liftEither $ FromPython.run path prog
  prog <- ToRestrictedPython.run prog
  (prog, format) <- ToCore.run prog
  prog <- ConvertCore.run prog
  (args, env) <- ValueCore.readValueIO format
  result <- EvaluateCore.run prog args
  ValueCore.writeValueIO format env result

runCPlusPlus :: FilePath -> ExceptT Error IO ()
runCPlusPlus _ = throwCommandLineError "cannot execute C++"

run :: Target -> FilePath -> ExceptT Error IO ()
run = \case
  PythonTarget -> runPython
  RestrictedPythonTarget -> runRestrictedPython
  CoreTarget -> runCore
  CPlusPlusTarget -> runCPlusPlus
