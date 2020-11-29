module Jikka.Subcommand.Convert (run) where

import Data.Text (Text)
import qualified Jikka.Converter.Core.Simplify as Simplify
import qualified Jikka.Converter.Python.Alpha as ConvertAlpha
import qualified Jikka.Converter.Python.Convert as FromParsed
import qualified Jikka.Converter.Python.ToCore as ToCore
import qualified Jikka.Converter.Python.TypeInfer as TypeInfer
import qualified Jikka.Deserializer.Python as FromPython
import qualified Jikka.Serializer.Python as ToPrettyPython
import qualified Jikka.Serializer.Show as ToShow

run :: FilePath -> Text -> Either String Text
run path input = do
  prog <- FromPython.run path input
  prog <- ConvertAlpha.run prog
  prog <- FromParsed.run prog
  prog <- TypeInfer.run prog
  prog <- ToCore.run prog
  prog <- Simplify.run prog
  ToShow.run prog
