module Jikka.Main.Subcommand.Convert (run) where

import Data.Text (Text)
import qualified Jikka.CPlusPlus.Convert as FromCore
import qualified Jikka.CPlusPlus.Format as FormatCPlusPlus
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Optimize as Optimize
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse as FromPython
import qualified Jikka.RestrictedPython.Convert as ToCore

run :: FilePath -> Text -> Either Error Text
run path input = flip evalAlphaT 0 $ do
  prog <- FromPython.run path input
  prog <- ToRestrictedPython.run prog
  prog <- ToCore.run prog
  prog <- Optimize.run prog
  prog <- FromCore.run prog
  FormatCPlusPlus.run prog
