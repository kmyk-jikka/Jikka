module Jikka.Main.Subcommand.Debug (run) where

import qualified Data.Text.IO as T (putStrLn, readFile)
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Format as FormatCore
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse as ParsePython
import qualified Jikka.RestrictedPython.Convert as ToCore

run :: FilePath -> ExceptT Error IO ()
run path = flip evalAlphaT 0 $ do
  prog <- liftIO $ T.readFile path
  prog <- ParsePython.run path prog
  prog <- ToRestrictedPython.run prog
  prog <- ToCore.run prog
  prog <- FormatCore.run prog
  liftIO $ T.putStrLn prog
