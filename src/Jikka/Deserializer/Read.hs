module Jikka.Deserializer.Read where

import Data.Text (Text, unpack)
import Jikka.Language.Python.Type (Program (..))
import Text.Read (readEither)

run :: FilePath -> Text -> Either String Program
run path input = readEither $ unpack input
