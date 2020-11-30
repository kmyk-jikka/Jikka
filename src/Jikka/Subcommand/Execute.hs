module Jikka.Subcommand.Execute (run) where

import Control.Monad.Except
import qualified Data.Text.IO as T (readFile)
import qualified Jikka.Converter.Core.CleanUp as CleanUp
import qualified Jikka.Converter.Core.MakeEager as MakeEager
import qualified Jikka.Converter.Python.Alpha as ConvertAlpha
import qualified Jikka.Converter.Python.FromParsed as FromParsed
import qualified Jikka.Converter.Python.ToCore as ToCore
import qualified Jikka.Converter.Python.TypeInfer as TypeInfer
import qualified Jikka.Deserializer.Python as FromPython
import qualified Jikka.Evaluator.Core as Evaluator

run :: FilePath -> ExceptT String IO ()
run path = do
  prog <- liftIO $ T.readFile path
  prog <- liftEither $ FromPython.run path prog
  prog <- liftEither $ ConvertAlpha.run prog
  prog <- liftEither $ FromParsed.run prog
  prog <- liftEither $ TypeInfer.run prog
  prog <- liftEither $ ToCore.run prog
  prog <- liftEither $ CleanUp.run prog
  prog <- liftEither $ MakeEager.run prog
  value <- Evaluator.run prog
  liftIO . putStrLn $ show value
