module Jikka.Main.Subcommand.Convert (run) where

import Data.Text (Text)
import qualified Jikka.CPlusPlus.Convert.FromCore as FromCore
import qualified Jikka.CPlusPlus.Format as FormatCPlusPlus
import qualified Jikka.Core.Convert.Optimize as Optimize
import qualified Jikka.Core.Convert.ValueApps as ValueApps
import qualified Jikka.Python.Convert.Alpha as ConvertAlpha
import qualified Jikka.Python.Convert.FromParsed as FromParsed
import qualified Jikka.Python.Parse as FromPython
import qualified Jikka.RestrictedPython.Convert.ToCore as ToCore
import qualified Jikka.RestrictedPython.Convert.TypeInfer as TypeInfer

run :: FilePath -> Text -> Either String Text
run path input = do
  prog <- FromPython.run path input
  prog <- ConvertAlpha.run prog
  prog <- FromParsed.run prog
  prog <- TypeInfer.run prog
  prog <- ToCore.run prog
  prog <- Optimize.run prog
  prog <- ValueApps.run prog
  prog <- FromCore.run prog
  FormatCPlusPlus.run prog
