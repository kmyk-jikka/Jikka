module Jikka.Python.Parse (run) where

import Data.Text (Text, unpack)
import Jikka.Python.Language.Expr (Program)
import qualified Jikka.Python.Parse.Lexer as L
import qualified Jikka.Python.Parse.Parser as P

run :: FilePath -> Text -> Either String Program
run _ input = do
  tokens <- L.run $ unpack input
  P.run tokens
