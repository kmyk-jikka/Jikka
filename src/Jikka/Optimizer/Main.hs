module Jikka.Optimizer.Main where

import qualified Jikka.Language.Type as J
import qualified Jikka.Optimizer.Type.Rich as R
import qualified Jikka.Optimizer.Type.Simple as S

run :: J.Program -> Either String J.Program
run = Right
