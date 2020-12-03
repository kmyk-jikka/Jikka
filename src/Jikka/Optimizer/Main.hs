module Jikka.Optimizer.Main where

import qualified Jikka.Language.Parsed.Type as J

run :: J.Program -> Either String J.Program
run = Right
