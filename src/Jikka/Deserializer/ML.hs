module Jikka.Deserializer.ML (run) where

import Data.Text (Text, unpack)
import qualified Jikka.Deserializer.ML.Converter as C
import qualified Jikka.Deserializer.ML.Lexer as L
import qualified Jikka.Deserializer.ML.Parser as P
import qualified Jikka.Language.Parsed.Type as J

run :: FilePath -> Text -> Either String J.Program
run path input = do
  tokens <- L.run $ unpack input
  parsed <- P.run tokens
  C.run parsed
