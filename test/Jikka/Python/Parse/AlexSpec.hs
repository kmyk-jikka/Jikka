{-# LANGUAGE OverloadedStrings #-}

module Jikka.Python.Parse.AlexSpec
  ( spec,
  )
where

import Jikka.Common.Error (Error)
import Jikka.Common.Location
import Jikka.Python.Parse.Alex (run)
import Jikka.Python.Parse.Token (Token (..))
import Test.Hspec

run' :: String -> Either Error [Token]
run' input = do
  tokens <- run input
  return $ map value tokens

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input = "abc ** 123"
    let tokens = [Ident "abc", PowOp, Int 123, Newline]
    run' input `shouldBe` Right tokens
  it "puts 1-based position info" $ do
    let input = "abc def\n123"
    let tokens = [WithLoc (Loc 1 1 3) $ Ident "abc", WithLoc (Loc 1 5 3) Def, WithLoc (Loc 1 8 1) Newline, WithLoc (Loc 2 1 3) $ Int 123, WithLoc (Loc 0 0 0) Newline]
    run input `shouldBe` Right tokens
  it "inserts <indent> tokens" $ do
    let input = "if:\n    return"
    let tokens = [If, Colon, Newline, Indent, Return, Newline, Dedent]
    run' input `shouldBe` Right tokens
  it "uses the longest match" $ do
    let input = "i in int ints"
    let tokens = [Ident "i", In, Ident "int", Ident "ints", Newline]
    run' input `shouldBe` Right tokens
