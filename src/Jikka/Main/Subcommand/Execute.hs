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
import qualified Jikka.RestrictedPython.Convert.ToCore as ToCore
import qualified Jikka.RestrictedPython.Convert.TypeInfer as TypeInfer

-- | TODO: remove this
liftEither' :: Either String a -> ExceptT Error IO a
liftEither' f = case f of
  Left msg -> throwError (Error msg)
  Right x -> return x

run :: FilePath -> ExceptT Error IO ()
run path = do
  let counter = 0
  prog <- liftIO $ T.readFile path
  prog <- liftEither $ FromPython.run path prog
  (prog, _) <- runAlphaT counter $ ToRestrictedPython.run prog
  prog <- liftEither' $ TypeInfer.run prog
  prog <- liftEither' $ ToCore.run prog
  prog <- liftEither' $ Optimize.run prog
  prog <- liftEither' $ MakeEager.run prog
  value <- Evaluator.run prog
  liftIO . putStrLn $ show value
