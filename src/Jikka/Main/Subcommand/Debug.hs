module Jikka.Main.Subcommand.Debug (run) where

import qualified Data.Text.IO as T (readFile)
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse as FromPython
import qualified Jikka.RestrictedPython.Convert as Convert
import qualified Jikka.RestrictedPython.Format as FormatRestrictedPython

run :: FilePath -> ExceptT Error IO ()
run path = flip evalAlphaT 0 $ do
  prog <- liftIO $ T.readFile path
  prog <- FromPython.run path prog
  prog <- ToRestrictedPython.run prog
  prog <- Convert.run' prog
  prog <- return $ FormatRestrictedPython.run' prog
  liftIO $ putStrLn prog
