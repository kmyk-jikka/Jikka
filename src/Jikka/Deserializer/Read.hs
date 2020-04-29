module Jikka.Deserializer.Read where

import Data.Text (Text, unpack)
import qualified Jikka.Language.Type as J
import Text.Read (readEither)

run :: FilePath -> Text -> Either String J.Expr
run path input = readEither $ unpack input
