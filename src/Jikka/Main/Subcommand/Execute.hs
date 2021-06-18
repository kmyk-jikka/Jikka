module Jikka.Main.Subcommand.Execute (run) where

import Control.Monad.Except
import qualified Data.Text.IO as T (readFile)
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.MakeEager as MakeEager
import qualified Jikka.Core.Convert.Optimize as Optimize
import qualified Jikka.Core.Evaluate as Evaluator
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse as FromPython
import qualified Jikka.RestrictedPython.Convert.Alpha as Alpha
import qualified Jikka.RestrictedPython.Convert.ToCore as ToCore
import qualified Jikka.RestrictedPython.Convert.TypeInfer as TypeInfer

run :: FilePath -> ExceptT Error IO ()
run path = flip evalAlphaT 0 $ do
  prog <- liftIO $ T.readFile path
  prog <- liftEither $ FromPython.run path prog
  prog <- ToRestrictedPython.run prog
  prog <- Alpha.run prog
  prog <- TypeInfer.run prog
  prog <- ToCore.run prog
  prog <- Optimize.run prog
  prog <- MakeEager.run prog
  value <- Evaluator.run prog
  liftIO $ print value
