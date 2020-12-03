module Jikka.Python.ParseSpec
  ( spec,
  )
where

import Data.Text (pack)
import Jikka.Common.Error (Error)
import Jikka.Common.Language.Name
import Jikka.Common.Language.Pos
import Jikka.Python.Language.Expr
import Jikka.Python.Parse
import Test.Hspec

at :: a -> (Int, Int) -> WithPos a
at a (y, x) = WithPos (Pos y x) a

run' :: [String] -> Either Error Program
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
