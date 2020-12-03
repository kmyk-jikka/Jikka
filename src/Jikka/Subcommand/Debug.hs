module Jikka.Subcommand.Debug (run) where

import Control.Monad.Except
import Data.Text (unpack)
import qualified Data.Text.IO as T (readFile)
import qualified Jikka.Converter.CPlusPlus.FromCore as FromCore
import qualified Jikka.Converter.Core.RemoveUnusedVars as RemoveUnusedVars
import qualified Jikka.Converter.Core.StrengthReduction as StrengthReduction
import qualified Jikka.Converter.Core.ValueApps as ValueApps
import qualified Jikka.Converter.Python.Alpha as ConvertAlpha
import qualified Jikka.Converter.Python.FromParsed as FromParsed
import qualified Jikka.Converter.Python.ToCore as ToCore
import qualified Jikka.Converter.Python.TypeInfer as TypeInfer
import qualified Jikka.Deserializer.Python.Lexer as PythonLexer
import qualified Jikka.Deserializer.Python.Parser as PythonParser
import qualified Jikka.Serializer.CPlusPlus as FormatCPlusPlus
import qualified Jikka.Serializer.Core as FormatCore
import qualified Jikka.Serializer.Python as FormatPython

put :: String -> String -> ExceptT String IO ()
put title message = do
  liftIO $ putStrLn (title ++ ":")
  let indent = unlines . map ("    " ++) . lines
  liftIO $ putStrLn (indent message)

run :: FilePath -> ExceptT String IO ()
run path = do
  put "path" $ show path
  prog <- liftIO $ T.readFile path
  put "input" $ unpack prog
  prog <- liftEither $ PythonLexer.run (unpack prog)
  put "tokens" $ unlines (map show prog)
  prog <- liftEither $ PythonParser.run prog
  put "parsed" $ show prog
  prog <- liftEither $ ConvertAlpha.run prog
  put "alpha converted" $ show prog
  prog <- liftEither $ FromParsed.run prog
  put "converted AT" . unpack =<< liftEither (FormatPython.run prog)
  prog <- liftEither $ TypeInfer.run prog
  put "infered types" . unpack =<< liftEither (FormatPython.run prog)
  prog <- liftEither $ ToCore.run prog
  put "core" . unpack =<< liftEither (FormatCore.run prog)
  prog <- liftEither $ RemoveUnusedVars.run prog
  put "core simplified" . unpack =<< liftEither (FormatCore.run prog)
  prog <- liftEither $ StrengthReduction.run prog
  put "core reduced" . unpack =<< liftEither (FormatCore.run prog)
  prog <- liftEither $ ValueApps.run prog
  put "simplify for codgen" . unpack =<< liftEither (FormatCore.run prog)
  prog <- liftEither $ FromCore.run prog
  put "generated code" . unpack =<< liftEither (FormatCPlusPlus.run prog)
  return ()
