module Jikka.Serializer.Show where

import Data.Text (Text, pack)
import qualified Jikka.Optimizer.Type.Simple as S

run :: S.Expr -> Either String Text
run expr = Right . pack $ show expr
