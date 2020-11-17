module Jikka.Deserializer.Python (run) where

import Data.Text (Text, unpack)
-- import qualified Jikka.Deserializer.Python.Formatter as C
import qualified Jikka.Deserializer.Python.Lexer as L
-- import qualified Jikka.Deserializer.Python.Parser as P
import qualified Jikka.Language.Python.Type as Py

run :: FilePath -> Text -> Either String Py.Program
run path input = do
  tokens <- L.run $ unpack input
  undefined
--   parsed <- P.run tokens
--   C.run parsed
