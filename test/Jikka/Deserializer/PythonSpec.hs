module Jikka.Deserializer.PythonSpec
  ( spec,
  )
where

import Data.Text (pack)
import Jikka.Deserializer.Python
import Jikka.Language.Common.Name
import Jikka.Language.Common.Pos
import Jikka.Language.Python.Parsed.Type
import Test.Hspec

at :: a -> (Int, Int) -> WithPos a
at a (y, x) = WithPos (Pos y x) a

run' :: [String] -> Either String Program
run' lines = run "test.py" (pack . concat $ map (++ "\n") lines)

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          [ "def solve() -> int:",
            "    return 42"
          ]
    let parsed = Program [FunDef (FunName "solve") [] (Just $ TyInt `at` (1, 16)) [Return (Lit (LitInt 42) `at` (2, 12)) `at` (2, 5)] `at` (1, 1)]
    run' input `shouldBe` Right parsed
