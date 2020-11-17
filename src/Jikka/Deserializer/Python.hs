module Jikka.Deserializer.Python (run) where

import Data.Text (Text, unpack)
import Jikka.Language.Python.Type

run :: FilePath -> Text -> Either String Program
run path input =
  return
    Program
      { isCompatImported = False,
        decls = []
      }
