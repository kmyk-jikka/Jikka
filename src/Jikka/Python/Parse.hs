module Jikka.Python.Parse (run) where

import Data.Text (Text, unpack)
import Jikka.Python.Language.Expr (Program)
import qualified Jikka.Python.Parse.Alex as L
import qualified Jikka.Python.Parse.Happy as P

run :: FilePath -> Text -> Either String Program
run _ input = do
  tokens <- L.run $ unpack input
  P.run tokens
