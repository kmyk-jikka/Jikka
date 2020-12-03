module Jikka.Serializer.Show where

import Data.Text (Text, pack)
import qualified Jikka.Language.Parsed.Type as J

run :: J.Program -> Either String Text
run e = Right . pack $ show e
