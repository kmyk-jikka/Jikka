{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Language.AssertedHintSpec (spec) where

import Data.Maybe
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.ArithmeticExpr as ArithmeticExpr
import qualified Jikka.Core.Convert.EqualitySolving as EqualitySolving
import qualified Jikka.Core.Convert.TypeInfer as TypeInfer
import Jikka.Core.Language.ArithmeticExpr
import Jikka.Core.Language.AssertedHint
import Jikka.Core.Language.Expr
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Parse (parseExpr)
import Test.Hspec

parseExpr' :: [(VarName, Type)] -> String -> Expr
parseExpr' env e = fromSuccess . flip evalAlphaT 100 $ do
  e <- parseExpr e
  e <- TypeInfer.runExpr env e
  e <- fromMaybe e <$> applyRewriteRule EqualitySolving.rule (makeRewriteEnvironmentFromTypeEnv env) e
  ArithmeticExpr.runExpr env e

spec :: Spec
spec = do
  describe "parseHints" $ do
    it "works on n == 3" $ do
      let e = parseExpr' [("n", IntTy)] "n == 3"
      let expected =
            [ ("n", EqualHint (integerArithmeticExpr 3))
            ]
      parseHints e `shouldBe` expected
    it "works on length xs == 3" $ do
      let e = parseExpr' [("xs", ListTy IntTy)] "len xs == 3"
      let expected =
            [ ("xs", LengthHint IntTy (EqualHint (integerArithmeticExpr 3)))
            ]
      parseHints e `shouldBe` expected
    it "works on length xs >= 3" $ do
      let e = parseExpr' [("xs", ListTy IntTy)] "len xs >= 3"
      let expected =
            [ ("xs", LengthHint IntTy (BoundHint (Just (integerArithmeticExpr 3)) Nothing))
            ]
      parseHints e `shouldBe` expected
    it "works on xs /= nil" $ do
      let e = parseExpr' [("xs", ListTy IntTy)] "xs /= nil"
      let expected =
            [ ("xs", LengthHint IntTy (BoundHint (Just (integerArithmeticExpr 1)) Nothing))
            ]
      parseHints e `shouldBe` expected
    it "works on length 0 <= xs - 3" $ do
      let e = parseExpr' [("xs", ListTy IntTy)] "0 <= len xs - 3"
      let expected =
            [ ("xs", LengthHint IntTy (BoundHint (Just (integerArithmeticExpr 3)) Nothing))
            ]
      parseHints e `shouldBe` expected

  describe "lowerBoundWithHints" $ do
    it "works on length n == 3" $ do
      let hints = [("n", EqualHint (integerArithmeticExpr 3))]
      let e = Var "n"
      let expected = Just 3
      lowerBoundWithHints hints e `shouldBe` expected

  describe "upperBoundWithHints" $ do
    it "works on length n == 3" $ do
      let hints = [("n", EqualHint (integerArithmeticExpr 3))]
      let e = Var "n"
      let expected = Just 4
      upperBoundWithHints hints e `shouldBe` expected

  describe "nullWithHints" $ do
    it "works on length xs >= 3" $ do
      let hints = [("xs", LengthHint IntTy (BoundHint (Just (integerArithmeticExpr 3)) Nothing))]
      let e = Var "xs"
      let expected = Just False
      nullWithHints hints e `shouldBe` expected
    it "works on length xs == 0" $ do
      let hints = [("xs", LengthHint IntTy (EqualHint (integerArithmeticExpr 0)))]
      let e = Var "xs"
      let expected = Just True
      nullWithHints hints e `shouldBe` expected
    it "works on length xs == 1" $ do
      let hints = [("xs", LengthHint IntTy (EqualHint (integerArithmeticExpr 1)))]
      let e = Var "xs"
      let expected = Just False
      nullWithHints hints e `shouldBe` expected
    it "works on length xs < 3" $ do
      let hints = [("xs", LengthHint IntTy (BoundHint Nothing (Just (integerArithmeticExpr 3))))]
      let e = Var "xs"
      let expected = Nothing
      nullWithHints hints e `shouldBe` expected
