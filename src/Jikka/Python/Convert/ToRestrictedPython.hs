module Jikka.Python.Convert.ToRestrictedPython (run) where

import qualified Jikka.Python.Language.Expr as X
import qualified Jikka.RestrictedPython.Language.Expr as Y

run :: X.Program -> Either String Y.Program
run = undefined
