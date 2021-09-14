{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Language.EtaSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.TypeInfer as TypeInfer
import Jikka.Core.Language.Eta
import Jikka.Core.Language.Expr
import Jikka.Core.Parse (parseExpr)
import Test.Hspec

etaExpand'' :: [(VarName, Type)] -> Expr -> Either Error (Maybe Expr)
etaExpand'' env = flip evalAlphaT 0 . etaExpand' env

parseExpr' :: [(VarName, Type)] -> [String] -> Expr
parseExpr' env = fromSuccess . flip evalAlphaT 100 . (TypeInfer.runExpr env <=< parseExpr . unlines)

spec :: Spec
spec = do
  describe "etaExpand'" $ do
    it "works on sum" $ do
      let env = []
      let e =
            parseExpr'
              env
              [ "sum"
              ]
      let expected =
            parseExpr'
              env
              [ "fun $0 -> sum $0"
              ]
      etaExpand'' env e `shouldBe` Right (Just expected)
    it "works on let" $ do
      let env = []
      let e =
            parseExpr'
              env
              [ "let f = fun x y -> x + y",
                "in f"
              ]
      let expected =
            parseExpr'
              env
              [ "fun $0 $1 ->",
                "    (let f = fun x y -> x + y",
                "    in f) $0 $1"
              ]
      etaExpand'' env e `shouldBe` Right (Just expected)
    it "works on a partial lambda" $ do
      let env = [("f", Fun3Ty IntTy BoolTy UnitTy (ListTy IntTy))]
      let e =
            parseExpr'
              env
              [ "fun (x: int) -> f x"
              ]
      let expected =
            parseExpr'
              env
              [ "fun (x: int) ($0: bool) ($1: unit) -> f x $0 $1"
              ]
      etaExpand'' env e `shouldBe` Right (Just expected)
    it "does nothing on integers" $ do
      let env = []
      let e =
            parseExpr'
              env
              [ "12"
              ]
      etaExpand'' env e `shouldBe` Right Nothing

  describe "etaReduce'" $ do
    it "works on sum" $ do
      let env = []
      let e =
            parseExpr'
              env
              [ "fun $0 -> sum $0"
              ]
      let expected =
            parseExpr'
              env
              [ "sum"
              ]
      etaReduce' e `shouldBe` Just expected
    it "works on an application which is longer than lambda abstraction" $ do
      let env = [("f", Fun3STy IntTy)]
      let e =
            parseExpr'
              env
              [ "fun x y -> f 12 x y"
              ]
      let expected =
            parseExpr'
              env
              [ "f 12"
              ]
      etaReduce' e `shouldBe` Just expected
    it "does nothing on a flipped application" $ do
      let env = [("f", Fun2STy IntTy)]
      let e =
            parseExpr'
              env
              [ "fun x y -> f y x"
              ]
      etaReduce' e `shouldBe` Nothing
