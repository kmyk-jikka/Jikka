module Jikka.Subcommand.Convert (run) where

import Data.Text (Text)
import qualified Jikka.Converter.CPlusPlus.FromCore as FromCore
import qualified Jikka.Converter.Core.Optimize as Optimize
import qualified Jikka.Converter.Core.ValueApps as ValueApps
import qualified Jikka.Converter.Python.Alpha as ConvertAlpha
import qualified Jikka.Converter.Python.FromParsed as FromParsed
import qualified Jikka.Converter.Python.ToCore as ToCore
import qualified Jikka.Converter.Python.TypeInfer as TypeInfer
import qualified Jikka.Deserializer.Python as FromPython
import qualified Jikka.Serializer.CPlusPlus as FormatCPlusPlus

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
