module Jikka.Deserializer.Python.Converter (run) where

import Jikka.Deserializer.Pos
import qualified Jikka.Language.Python.Parsed.Type as X
import qualified Jikka.Language.Python.Typed.Type as Y

run :: X.Program -> Either String Y.Program
run _ = return Y.Program
