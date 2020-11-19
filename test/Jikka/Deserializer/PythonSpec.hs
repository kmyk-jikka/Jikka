module Jikka.Deserializer.PythonSpec
  ( spec,
  )
where

import Data.Text (Text, pack)
import Jikka.Deserializer.Python
import Jikka.Language.Python.Parsed.Type
import Test.Hspec

spec :: Spec
spec = describe "run" $ return ()
{-
run' :: [String] -> Either String Program
run' lines = run "test.py" (pack . concat $ map (++ "\n") lines)

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          [ "def solve() -> int:",
            "    return 42"
          ]
    let parsed = Program [FunDef (FunName "solve") [] TyInt [Return (Lit (LitInt 42))]]
    run' input `shouldBe` Right parsed
  it "works on a small fun def" $ do
    let input =
          [ "def solve(p: bool) -> int:",
            "    if p:",
            "        return 0",
            "    else:",
            "        return 1"
          ]
    let parsed =
          Program
            [FunDef (FunName "solve") [(VarName "p", TyBool)] TyInt [If (Var (VarName "p")) [Return (Lit (LitInt 0))] [Return (Lit (LitInt 1))]]]
    run' input `shouldBe` Right parsed
-}
