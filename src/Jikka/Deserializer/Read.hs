module Jikka.Deserializer.Read where

import Data.Text (Text, unpack)
import Jikka.Optimizer.Type.Rich as R
import Text.Read (readEither)

run :: FilePath -> Text -> Either String R.Expr
run path input = readEither $ unpack input
