{-# LANGUAGE OverloadedStrings #-}

module Jikka.Common.Parse.OffsideRuleSpec
  ( spec,
  )
where

import Data.Either (isLeft)
import Jikka.Common.Language.Pos
import Jikka.Common.Parse.OffsideRule (IndentSetting (..), insertIndentTokens)
import Test.Hspec

token :: Int -> Int -> a -> WithPos a
token line column = WithPos (Pos line column)

indent :: String
indent = "<indent>"

dedent :: String
dedent = "<dedent>"

open :: String
open = "<open>"

close :: String
close = "<close>"

run :: [WithPos String] -> Either String [String]
run tokens = map value <$> insertIndentTokens setting tokens
  where
    setting :: IndentSetting String
    setting =
      IndentSetting
        { indentToken = indent,
          dedentToken = dedent,
          initialLine = 0,
          initialColumn = 0,
          isOpenParenToken = (== open),
          isCloseParenToken = (== close),
          allowNoMatchingDedent = False
        }

spec :: Spec
spec = describe "insertIndentTokens" $ do
  it "works" $ do
    let tokens = [token 0 0 "def", token 0 4 "main():", token 1 4 "print(\"Hello, world!\")"]
    let inserted = ["def", "main():", indent, "print(\"Hello, world!\")", dedent]
    run tokens `shouldBe` Right inserted
  it "works'" $ do
    let tokens = [token 0 0 "def", token 0 4 "main():", token 1 4 "print(\"Hello, world!\")", token 2 0 "if __name__ == '__main__':", token 3 4 "main()"]
    let inserted = ["def", "main():", indent, "print(\"Hello, world!\")", dedent, "if __name__ == '__main__':", indent, "main()", dedent]
    run tokens `shouldBe` Right inserted
  it "inserts all <dedent> when it reaches EOF during indented" $ do
    let tokens = [token 0 0 "a", token 1 4 "b", token 2 8 "c", token 3 12 "d"]
    let inserted = ["a", indent, "b", indent, "c", indent, "d", dedent, dedent, dedent]
    run tokens `shouldBe` Right inserted
  it "ignores blank lines" $ do
    let tokens = [token 10 0 "foo", token 20 0 "bar"]
    let inserted = ["foo", "bar"]
    run tokens `shouldBe` Right inserted
  it "doesn't insert <dedent> for blank lines" $ do
    let tokens = [token 0 0 "baz", token 10 4 "foo", token 20 4 "bar"]
    let inserted = ["baz", indent, "foo", "bar", dedent]
    run tokens `shouldBe` Right inserted
  it "does nothing without indent" $ do
    let tokens = [token 0 0 "a", token 1 0 "b", token 2 0 "c"]
    let inserted = ["a", "b", "c"]
    run tokens `shouldBe` Right inserted
  it "inserts <indent> when the first line is indented" $ do
    let tokens = [token 0 4 "a", token 1 8 "b", token 2 4 "c"]
    let inserted = [indent, "a", indent, "b", dedent, "c", dedent]
    run tokens `shouldBe` Right inserted
  it "fails when there is no matching <indent> for <dedent> (allowNoMatchingDedent = False)" $ do
    let tokens = [token 0 0 "a", token 1 8 "b", token 2 4 "c"]
    run tokens `shouldSatisfy` isLeft
  it "doesn't insert <indent> in parens" $ do
    let tokens = [token 0 0 "a", token 0 4 open, token 1 8 "b", token 2 4 "c", token 3 0 close, token 4 4 "d"]
    let inserted = ["a", open, "b", "c", close, indent, "d", dedent]
    run tokens `shouldBe` Right inserted
  it "doesn't blame about broken parens" $ do
    let tokens = [token 0 0 close]
    let inserted = [close]
    run tokens `shouldBe` Right inserted
