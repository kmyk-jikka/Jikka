module Jikka.Deserializer.Python (run) where

import Data.Text (Text, unpack)
import qualified Jikka.Deserializer.Python.Lexer as L
import qualified Jikka.Deserializer.Python.Parser as P
import Jikka.Language.Python.Parsed.Expr (Program)

run :: FilePath -> Text -> Either String Program
run path input = do
  tokens <- L.run $ unpack input
  P.run tokens
