module Jikka.Serializer.Show where

import Data.Text (Text, pack)

run :: Show program => program -> Either String Text
run e = Right . pack $ show e
