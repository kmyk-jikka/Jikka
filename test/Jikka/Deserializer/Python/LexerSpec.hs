{-# LANGUAGE OverloadedStrings #-}

module Jikka.Deserializer.Python.LexerSpec
  ( spec,
  )
where

import Jikka.Deserializer.Python.Lexer (Token (..), run)
import Jikka.Language.Common.Pos
import Test.Hspec

run' :: String -> Either String [Token]
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
    let tokens = [WithPos (Pos 1 1) $ Ident "abc", WithPos (Pos 1 5) Def, WithPos (Pos 1 8) Newline, WithPos (Pos 2 1) $ Int 123]
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
