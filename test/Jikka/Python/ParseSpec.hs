{-# LANGUAGE OverloadedStrings #-}

module Jikka.Python.ParseSpec
  ( spec,
  )
where

import Data.Text (pack)
import Jikka.Common.Error (Error)
import Jikka.Common.Location
import Jikka.Python.Language.Expr
import Jikka.Python.Language.Util
import Jikka.Python.Parse
import Test.Hspec

at :: a -> (Int, Int, Int) -> WithLoc a
at a (y, x, width) = WithLoc (Loc y x width) a

run' :: String -> Either Error Program
run' prog = run "test.py" (pack prog)

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          unlines
            [ "def solve() -> int:",
              "    return 42"
            ]
    let parsed = [FunctionDef ("solve" `at` (1, 5, 5)) emptyArguments [Return (Just (constIntExp 42 `at` (2, 12, 2))) `at` (2, 5, 6)] [] (Just (Name ("int" `at` (1, 16, 3)) `at` (1, 16, 3))) `at` (1, 1, 3)]
    run' input `shouldBe` Right parsed
  it "works even without trailing newline" $ do
    let input = "def solve() -> int:\n    return 42"
    let parsed = [FunctionDef ("solve" `at` (1, 5, 5)) emptyArguments [Return (Just (constIntExp 42 `at` (2, 12, 2))) `at` (2, 5, 6)] [] (Just (Name ("int" `at` (1, 16, 3)) `at` (1, 16, 3))) `at` (1, 1, 3)]
    run' input `shouldBe` Right parsed
  it "works even with CRLF" $ do
    let input = "def solve() -> int:\r\n    return 42\r\n"
    let parsed = [FunctionDef ("solve" `at` (1, 5, 5)) emptyArguments [Return (Just (constIntExp 42 `at` (2, 12, 2))) `at` (2, 5, 6)] [] (Just (Name ("int" `at` (1, 16, 3)) `at` (1, 16, 3))) `at` (1, 1, 3)]
    run' input `shouldBe` Right parsed
