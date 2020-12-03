{-# LANGUAGE OverloadedStrings #-}

module Jikka.Deserializer.ML.LexerSpec
  ( spec,
  )
where

import Jikka.Deserializer.ML.Lexer
import Jikka.Deserializer.ML.Pos
import Test.Hspec

run' :: String -> Either String [Token]
run' input = do
  tokens <- run input
  return $ map value tokens

spec :: Spec
spec = describe "lexer" $ do
  it "simple" $ do
    let input = "a + b"
    let tokens = [Ident "a", Op "+", Ident "b"]
    run' input `shouldBe` Right tokens
  it "let" $ do
    let input = "let x = 0 in x"
    let tokens = [Let, Ident "x", Equal, Int 0, In, Ident "x"]
    run' input `shouldBe` Right tokens
  it "longest match" $ do
    let input = "i in int ints"
    let tokens = [Ident "i", In, Ident "int", Ident "ints"]
    run' input `shouldBe` Right tokens
  it "shunting-yard algorithm" $ do
    let input = "(x + 1) ** 2 == x ** 2 + 2 * x + 1"
    let tokens = [OpenParen, Ident "x", Op "+", Int 1, CloseParen, Op "**", Int 2, Op "==", Ident "x", Op "**", Int 2, Op "+", Int 2, Op "*", Ident "x", Op "+", Int 1]
    run' input `shouldBe` Right tokens
