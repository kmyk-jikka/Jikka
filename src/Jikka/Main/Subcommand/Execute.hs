module Jikka.Main.Subcommand.Execute (run) where

import Control.Monad.Except
import qualified Data.Text.IO as T (readFile)
import qualified Jikka.Core.Convert.MakeEager as MakeEager
import qualified Jikka.Core.Convert.Optimize as Optimize
import qualified Jikka.Core.Evaluate as Evaluator
import qualified Jikka.Python.Convert.Alpha as ConvertAlpha
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse as FromPython
import qualified Jikka.RestrictedPython.Convert.ToCore as ToCore
import qualified Jikka.RestrictedPython.Convert.TypeInfer as TypeInfer

run :: FilePath -> ExceptT String IO ()
run path = do
  prog <- liftIO $ T.readFile path
  prog <- liftEither $ FromPython.run path prog
  prog <- liftEither $ ConvertAlpha.run prog
  prog <- liftEither $ ToRestrictedPython.run prog
  prog <- liftEither $ TypeInfer.run prog
  prog <- liftEither $ ToCore.run prog
  prog <- liftEither $ Optimize.run prog
  prog <- liftEither $ MakeEager.run prog
  value <- Evaluator.run prog
  liftIO . putStrLn $ show value
