module Jikka.Common.Parse.Read where

import Data.Text (Text, unpack)
import Text.Read (readEither)

run :: Read program => FilePath -> Text -> Either String program
run _ input = readEither $ unpack input
