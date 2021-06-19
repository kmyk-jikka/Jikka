module Jikka.Main.Subcommand.Convert (run) where

import Data.Text (Text)
import qualified Jikka.CPlusPlus.Convert.FromCore as FromCore
import qualified Jikka.CPlusPlus.Format as FormatCPlusPlus
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.ANormal as ANormal
import qualified Jikka.Core.Convert.Optimize as Optimize
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse as FromPython
import qualified Jikka.RestrictedPython.Convert.Alpha as Alpha
import qualified Jikka.RestrictedPython.Convert.ToCore as ToCore
import qualified Jikka.RestrictedPython.Convert.TypeInfer as TypeInfer

run :: FilePath -> Text -> Either Error Text
run path input = flip evalAlphaT 0 $ do
  prog <- FromPython.run path input
  prog <- ToRestrictedPython.run prog
  prog <- Alpha.run prog
  prog <- TypeInfer.run prog
  prog <- ToCore.run prog
  prog <- Optimize.run prog
  prog <- ANormal.run prog
  prog <- FromCore.run prog
  FormatCPlusPlus.run prog
