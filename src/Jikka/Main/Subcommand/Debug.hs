module Jikka.Main.Subcommand.Debug (run) where

import Data.Text (unpack)
import qualified Data.Text.IO as T (readFile)
import qualified Jikka.CPlusPlus.Convert.FromCore as FromCore
import qualified Jikka.CPlusPlus.Format as FormatCPlusPlus
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.ANormal as ANormal
import qualified Jikka.Core.Convert.RemoveUnusedVars as RemoveUnusedVars
import qualified Jikka.Core.Convert.StrengthReduction as StrengthReduction
import qualified Jikka.Core.Format as FormatCore
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse.Alex as PythonLexer
import qualified Jikka.Python.Parse.Happy as PythonParser
import qualified Jikka.RestrictedPython.Convert.Alpha as Alpha
import qualified Jikka.RestrictedPython.Convert.ToCore as ToCore
import qualified Jikka.RestrictedPython.Convert.TypeInfer as TypeInfer
import qualified Jikka.RestrictedPython.Format as FormatRestrictedPython

put :: MonadIO m => String -> String -> m ()
put title message = do
  liftIO $ putStrLn (title ++ ":")
  let indent = unlines . map ("    " ++) . lines
  liftIO $ putStrLn (indent message)

run :: FilePath -> ExceptT Error IO ()
run path = evalAlphaT 0 $ do
  put "path" $ show path
  prog <- liftIO $ T.readFile path
  put "input" $ unpack prog
  prog <- PythonLexer.run (unpack prog)
  put "tokens" $ unlines (map show prog)
  prog <- PythonParser.run prog
  put "parsed" $ show prog
  prog <- ToRestrictedPython.run prog
  put "restricted" . unpack =<< FormatRestrictedPython.run prog
  prog <- Alpha.run prog
  put "alpha" . unpack =<< FormatRestrictedPython.run prog
  prog <- TypeInfer.run prog
  put "infered types" . unpack =<< FormatRestrictedPython.run prog
  prog <- ToCore.run prog
  put "core" . unpack =<< FormatCore.run prog
  prog <- RemoveUnusedVars.run prog
  put "core simplified" . unpack =<< FormatCore.run prog
  prog <- StrengthReduction.run prog
  put "core reduced" . unpack =<< FormatCore.run prog
  prog <- ANormal.run prog
  put "simplify for codgen" . unpack =<< FormatCore.run prog
  prog <- FromCore.run prog
  put "generated code" . unpack =<< FormatCPlusPlus.run prog
  return ()
