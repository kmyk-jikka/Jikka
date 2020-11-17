module Jikka.Serializer.Show where

import Data.Text (Text, pack)
import Jikka.Language.Python.Type (Program (..))

run :: Program -> Either String Text
run e = Right . pack $ show e
