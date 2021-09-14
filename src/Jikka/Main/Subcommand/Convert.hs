{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Subcommand.Convert
-- Description : is the entry point of @convert@ subcommand. / @convert@ サブコマンドのエントリポイントです。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Main.Subcommand.Convert (run) where

import Data.Text (Text, pack)
import qualified Jikka.CPlusPlus.Convert as FromCore
import qualified Jikka.CPlusPlus.Convert.BurnFlavouredNames as BurnFlavouredNamesCPlusPlus
import qualified Jikka.CPlusPlus.Format as FormatCPlusPlus
import qualified Jikka.CPlusPlus.Language.Expr as CPlusPlus
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert as Convert
import qualified Jikka.Core.Format as FormatCore
import qualified Jikka.Core.Language.Expr as Core
import qualified Jikka.Core.Parse as ParseCore
import Jikka.Main.Target
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Language.Expr as Python
import qualified Jikka.Python.Parse as ParsePython
import qualified Jikka.RestrictedPython.Convert as ToCore
import qualified Jikka.RestrictedPython.Format as FormatRestrictedPython
import qualified Jikka.RestrictedPython.Language.Expr as RestrictedPython

data Program
  = PythonProgram Python.Program
  | RestrictedPythonProgram RestrictedPython.Program
  | CoreProgram Core.Program
  | CPlusPlusProgram CPlusPlus.Program
  deriving (Eq, Ord, Read, Show)

parseProgram :: (MonadAlpha m, MonadError Error m) => Target -> FilePath -> Text -> m Program
parseProgram source path input = case source of
  PythonTarget -> PythonProgram <$> ParsePython.run path input
  RestrictedPythonTarget -> throwCommandLineError "cannot convert from restricted Python"
  CoreTarget -> CoreProgram <$> ParseCore.run path input
  CPlusPlusTarget -> throwCommandLineError "cannot convert from C++"

convertProgram :: (MonadAlpha m, MonadError Error m) => Program -> Target -> m Program
convertProgram prog target = case (prog, target) of
  (PythonProgram _, PythonTarget) -> return prog
  (RestrictedPythonProgram _, RestrictedPythonTarget) -> return prog
  (CoreProgram prog, CoreTarget) -> CoreProgram <$> Convert.run prog -- optimize
  (CPlusPlusProgram _, CPlusPlusTarget) -> return prog
  (RestrictedPythonProgram _, PythonTarget) -> throwCommandLineError "cannot convert from restricted Python to Python"
  (CoreProgram _, PythonTarget) -> throwCommandLineError "cannot convert from core to Python"
  (CoreProgram _, RestrictedPythonTarget) -> throwCommandLineError "cannot convert from core to restricted Python"
  (PythonProgram prog, _) -> do
    prog <- ToRestrictedPython.run prog
    convertProgram (RestrictedPythonProgram prog) target
  (RestrictedPythonProgram prog, CoreTarget) -> do
    (prog, _) <- ToCore.run prog
    CoreProgram <$> Convert.run prog
  (RestrictedPythonProgram prog, CPlusPlusTarget) -> do
    (prog, format) <- ToCore.run prog
    prog <- Convert.run prog
    CPlusPlusProgram <$> FromCore.run prog (Just format)
  (CoreProgram prog, CPlusPlusTarget) -> do
    prog <- Convert.run prog
    CPlusPlusProgram <$> FromCore.run prog Nothing
  (CPlusPlusProgram _, _) -> throwCommandLineError "cannot convert from C++"

formatProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Text
formatProgram = \case
  PythonProgram prog -> return . pack $ show prog -- TODO
  RestrictedPythonProgram prog -> FormatRestrictedPython.run prog
  CoreProgram prog -> FormatCore.run prog
  CPlusPlusProgram prog -> do
    prog <- BurnFlavouredNamesCPlusPlus.run prog
    FormatCPlusPlus.run prog

run :: Target -> Target -> FilePath -> Text -> Either Error Text
run source target path input = flip evalAlphaT 0 $ do
  prog <- parseProgram source path input
  prog <- convertProgram prog target
  formatProgram prog
