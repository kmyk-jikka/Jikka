module Jikka.Deserializer.ML (Jikka.Deserializer.ML.run) where

import Data.Text (Text, unpack)
import Jikka.Deserializer.ML.Lexer as L
import Jikka.Deserializer.ML.Parser as P
import Jikka.Optimizer.Type.Interface as I

run :: FilePath -> Text -> Either String I.Expr
run path input = do
  tokens <- L.run $ unpack input
  parsed <- P.run tokens
  undefined
