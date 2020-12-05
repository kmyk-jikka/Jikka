{-# LANGUAGE OverloadedStrings #-}

module Jikka.Common.Parse.JoinLinesSpec
  ( spec,
  )
where

import Data.Either (isLeft)
import Jikka.Common.Error
import Jikka.Common.Location
import Jikka.Common.Parse.JoinLines
import Test.Hspec

open :: String
open = "<open>"

close :: String
close = "<close>"

newline :: String
newline = "<newline>"

joinLinesWithParens' :: [String] -> Either Error [String]
joinLinesWithParens' = post . go . pre
  where
    pre = map (WithLoc (Loc 1 0 (-1)))
    go :: [WithLoc String] -> Either Error [WithLoc String]
    go = joinLinesWithParens (== open) (== close) (== newline)
    post = fmap (map value)

removeEmptyLines' :: [String] -> [String]
removeEmptyLines' = post . go . pre
  where
    pre = map (WithLoc (Loc 0 0 (-1)))
    go :: [WithLoc String] -> [WithLoc String]
    go = removeEmptyLines (== newline)
    post = map value

spec :: Spec
spec = do
  describe "joinLinesWithParens" $ do
    it "works" $ do
      let tokens = ["f", newline, open, newline, "x", ",", newline, "y", close, newline]
      let expected = ["f", newline, open, "x", ",", "y", close, newline]
      joinLinesWithParens' tokens `shouldBe` Right expected
    it "reports unmatching parens" $ do
      let tokens = ["f", open, newline, "x"]
      joinLinesWithParens' tokens `shouldSatisfy` isLeft
  describe "removeEmptyLines" $ do
    it "works" $ do
      let tokens = [newline, "x", newline, newline, "y", newline]
      let expected = ["x", newline, "y", newline]
      removeEmptyLines' tokens `shouldBe` expected
