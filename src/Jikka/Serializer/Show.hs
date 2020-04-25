module Jikka.Serializer.Show where

import Data.Text (Text, pack)
import qualified Jikka.Optimizer.Type.Interface as I

run :: I.Expr -> Either String Text
run e = Right . pack $ show e
