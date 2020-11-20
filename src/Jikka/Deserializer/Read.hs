module Jikka.Deserializer.Read where

import Data.Text (Text, unpack)
import Text.Read (readEither)

run :: Read program => FilePath -> Text -> Either String program
run path input = readEither $ unpack input
