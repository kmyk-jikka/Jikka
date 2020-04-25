module Jikka.Optimizer.Main where

import qualified Jikka.Optimizer.Type.Interface as I
import qualified Jikka.Optimizer.Type.Rich as R
import qualified Jikka.Optimizer.Type.Simple as S

run :: I.Expr -> Either String I.Expr
run = Right
