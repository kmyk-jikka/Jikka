{-# LANGUAGE OverloadedStrings #-}

module Jikka.Common.Parse.OffsideRuleSpec
  ( spec,
  )
where

import Data.Either (isLeft)
import Jikka.Common.Error (Error)
import Jikka.Common.Location
import Jikka.Common.Parse.OffsideRule (insertIndents)
import Test.Hspec

indent :: String
indent = "<indent>"

dedent :: String
dedent = "<dedent>"

newline :: String
newline = "<newline>"

-- | This takes only `column` because `insertIndents` doesn't use `line` values.
token :: String -> Int -> WithLoc String
token s x = WithLoc (Loc 0 x 0) s

run :: [WithLoc String] -> Either Error [String]
run = post . go
  where
    go :: [WithLoc String] -> Either Error [WithLoc String]
    go = insertIndents indent dedent (== newline)
    post = fmap (map value)

spec :: Spec
spec = describe "insertIndentTokens" $ do
  it "works" $ do
    let tokens =
          concat
            [ [token "if:" 1, token newline 4],
              [token "return" 5, token newline 11],
              [token "else:" 1, token newline 6],
              [token "return:" 5, token newline 11]
            ]
    let expected =
          concat
            [ ["if:", newline],
              [indent, "return", newline],
              [dedent, "else:", newline],
              [indent, "return:", newline],
              [dedent]
            ]
    run tokens `shouldBe` Right expected
  it "fails on unmatching dedents" $ do
    let tokens =
          concat
            [ [token "if:" 1, token newline 4],
              [token "return" 5, token newline 11],
              [token "err" 3]
            ]
    run tokens `shouldSatisfy` isLeft
