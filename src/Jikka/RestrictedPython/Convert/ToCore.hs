module Jikka.RestrictedPython.Convert.ToCore
  ( run,
  )
where

import qualified Jikka.Core.Language.Expr as Y
import qualified Jikka.RestrictedPython.Language.Expr as X

run :: X.Program -> Either String Y.Program
run = undefined
