module Jikka.Deserializer.Read where

import Data.Text (Text, unpack)
import qualified Jikka.Optimizer.Type.Interface as I
import Text.Read (readEither)

run :: FilePath -> Text -> Either String I.Expr
run path input = readEither $ unpack input
