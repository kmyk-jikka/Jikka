module Jikka.Optimizer.Main where

import qualified Jikka.Optimizer.Type.Rich as R
import qualified Jikka.Optimizer.Type.Simple as S

run :: R.Expr -> Either String S.Expr
run = Right
