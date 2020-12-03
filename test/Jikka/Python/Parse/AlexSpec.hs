{-# LANGUAGE OverloadedStrings #-}

module Jikka.Python.Parse.AlexSpec
  ( spec,
  )
where

import Jikka.Common.Error (Error)
import Jikka.Common.Location
import Jikka.Python.Parse.Alex (Token (..), run)
import Test.Hspec

run' :: String -> Either Error [Token]
run' input = do
  tokens <- run input
  return $ map value tokens

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input = "abc ** 123"
    let tokens = [Ident "abc", Op "**", Int 123]
    run' input `shouldBe` Right tokens
  it "puts 1-based position info" $ do
    let input = "abc def\n123"
    let tokens = [WithLoc (Loc 1 1 3) $ Ident "abc", WithLoc (Loc 1 5 3) Def, WithLoc (Loc 1 8 1) Newline, WithLoc (Loc 2 1 3) $ Int 123]
    run input `shouldBe` Right tokens
  it "inserts <indent> tokens" $ do
    let input = "if:\n    return"
    let tokens = [If, Colon, Newline, Indent, Return, Dedent]
    run' input `shouldBe` Right tokens
  it "uses the longest match" $ do
    let input = "i in int ints"
    let tokens = [Ident "i", In, Ident "int", Ident "ints"]
    run' input `shouldBe` Right tokens
  it "accepts quotes" $ do
    let input = "'\"'\"'\"'"
    let tokens = [SingleQuote, DoubleQuote, SingleQuote, DoubleQuote, SingleQuote, DoubleQuote, SingleQuote]
    run' input `shouldBe` Right tokens
