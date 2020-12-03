module Jikka.Serializer.CPlusPlus (run) where

import Data.Text (Text, pack)
import qualified Jikka.Language.Parsed.Type as J
import qualified Jikka.Serializer.CPlusPlus.Converter as C
import qualified Jikka.Serializer.CPlusPlus.Pretty as P

run :: J.Program -> Either String Text
run prog = do
  prog' <- C.run prog
  output <- P.run prog'
  return $ pack output
