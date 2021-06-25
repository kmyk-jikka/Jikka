{-# LANGUAGE LambdaCase #-}

module Jikka.Main.Subcommand.Convert (run) where

import Data.Text (Text, pack)
import qualified Jikka.CPlusPlus.Convert as FromCore
import qualified Jikka.CPlusPlus.Format as FormatCPlusPlus
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert as Convert
import qualified Jikka.Core.Format as FormatCore
import Jikka.Main.Target
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse as ParsePython
import qualified Jikka.RestrictedPython.Convert as ToCore
import qualified Jikka.RestrictedPython.Format as FormatRestrictedPython

runPython :: FilePath -> Text -> Either Error Text
runPython path input = flip evalAlphaT 0 $ do
  prog <- ParsePython.run path input
  return . pack $ show prog -- TODO

runRestrictedPython :: FilePath -> Text -> Either Error Text
runRestrictedPython path input = flip evalAlphaT 0 $ do
  prog <- ParsePython.run path input
  prog <- ToRestrictedPython.run prog
  prog <- ToCore.run' prog
  FormatRestrictedPython.run prog

runCore :: FilePath -> Text -> Either Error Text
runCore path input = flip evalAlphaT 0 $ do
  prog <- ParsePython.run path input
  prog <- ToRestrictedPython.run prog
  prog <- ToCore.run prog
  prog <- Convert.run prog
  FormatCore.run prog

runCPlusPlus :: FilePath -> Text -> Either Error Text
runCPlusPlus path input = flip evalAlphaT 0 $ do
  prog <- ParsePython.run path input
  prog <- ToRestrictedPython.run prog
  prog <- ToCore.run prog
  prog <- Convert.run prog
  prog <- FromCore.run prog
  FormatCPlusPlus.run prog

run :: Target -> FilePath -> Text -> Either Error Text
run = \case
  PythonTarget -> runPython
  RestrictedPythonTarget -> runRestrictedPython
  CoreTarget -> runCore
  CPlusPlusTarget -> runCPlusPlus
