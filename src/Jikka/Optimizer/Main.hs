module Jikka.Optimizer.Main where

import qualified Jikka.Language.Type as J
import qualified Jikka.Optimizer.Type.Rich as R
import qualified Jikka.Optimizer.Type.Simple as S

run :: J.Expr -> Either String J.Expr
run = Right
