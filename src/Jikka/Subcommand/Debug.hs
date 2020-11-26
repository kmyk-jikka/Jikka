module Jikka.Subcommand.Debug (run) where

import Control.Monad.Except
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T (readFile)
import qualified Jikka.Converter.Optimizer as Opt
import qualified Jikka.Converter.Python.Alpha as ConvertAlpha
import qualified Jikka.Converter.Python.Convert as FromParsed
import qualified Jikka.Converter.Python.TypeInfer as TypeInfer
import qualified Jikka.Deserializer.Python.Lexer as PythonLexer
import qualified Jikka.Deserializer.Python.Parser as PythonParser
import qualified Jikka.Serializer.Python as ToPrettyPython

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
  put "converted AST" $ show prog
  prog <- liftEither $ TypeInfer.run prog
  put "infered types" $ show prog
  put "pretty printed" . unpack =<< liftEither (ToPrettyPython.run prog)
  return ()
