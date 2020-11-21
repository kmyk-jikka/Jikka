module Jikka.Subcommand.Execute (run) where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import qualified Jikka.Converter.Optimizer as Opt
import qualified Jikka.Converter.Python.Alpha as ConvertAlpha
import qualified Jikka.Converter.Python.Convert as FromParsed
import qualified Jikka.Deserializer.Python as FromPython

run :: FilePath -> ExceptT String IO ()
run path = do
  prog <- liftIO $ T.readFile path
  prog <- liftEither $ FromPython.run path prog
  prog <- liftEither $ ConvertAlpha.run prog
  prog <- liftEither $ FromParsed.run prog
  liftIO . putStrLn $ show prog
