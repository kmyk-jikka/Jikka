module Jikka.Serializer.Show where

import Data.Text (Text, pack)
import qualified Jikka.Language.Type as J

run :: J.Expr -> Either String Text
run e = Right . pack $ show e
