{-# LANGUAGE OverloadedStrings #-}

module Jikka.Python.ParseSpec
  ( spec,
  )
where

import Data.Text (pack)
import Jikka.Common.Error (Error)
import Jikka.Common.Location
import Jikka.Python.Language.Expr
import Jikka.Python.Parse
import Test.Hspec

at :: a -> (Int, Int, Int) -> WithLoc a
at a (y, x, width) = WithLoc (Loc y x width) a

run' :: [String] -> Either Error Program
run' lines = run "test.py" (pack $ unlines lines)

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          [ "def solve() -> int:",
            "    return 42"
          ]
    let parsed = [FunctionDef ("solve" `at` (1, 5, 5)) emptyArguments [Return (Just (Constant (ConstInt 42) `at` (2, 12, 2))) `at` (2, 5, 6)] [] (Just (Name ("int" `at` (1, 16, 3)) `at` (1, 16, 3))) `at` (1, 1, 3)]
    run' input `shouldBe` Right parsed
