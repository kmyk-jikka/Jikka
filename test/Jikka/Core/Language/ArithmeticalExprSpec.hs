{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Language.ArithmeticalExprSpec
  ( spec,
  )
where

import qualified Data.Vector as V
import Jikka.Core.Language.ArithmeticalExpr
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

spec :: Spec
spec = do
  describe "parseProductExpr" $ do
    it "works" $ do
      let e = Mult' (LitInt' 3) (Var "y")
      let expected = ProductExpr {productExprConst = 3, productExprList = [Var "y"]}
      parseProductExpr e `shouldBe` expected
  describe "multSumExpr" $ do
    it "may introduce empty ProductExpr" $ do
      let e1 = parseSumExpr (LitInt' 3)
      let e2 = parseSumExpr (Var "y")
      let expected =
            SumExpr
              { sumExprList =
                  [ ProductExpr {productExprConst = 0, productExprList = []},
                    ProductExpr {productExprConst = 3, productExprList = [Var "y"]}
                  ],
                sumExprConst = 0
              }
      multSumExpr e1 e2 `shouldBe` expected
  describe "parseArithmeticalExpr" $ do
    it "works" $ do
      let e = Plus' (Var "x") (Minus' (Mult' (LitInt' 3) (Var "y")) (Plus' (Var "x") (LitInt' 10)))
      let expected =
            SumExpr
              { sumExprList =
                  [ ProductExpr {productExprConst = 1, productExprList = [Var "x"]},
                    ProductExpr {productExprConst = 0, productExprList = []},
                    ProductExpr {productExprConst = 3, productExprList = [Var "y"]},
                    ProductExpr {productExprConst = -1, productExprList = [Var "x"]}
                  ],
                sumExprConst = -10
              }
      parseArithmeticalExpr e `shouldBe` expected
  describe "makeVectorFromArithmeticalExpr" $ do
    it "works" $ do
      let xs = V.fromList ["x", "y"]
      let e =
            parseArithmeticalExpr
              (Plus' (Var "x") (Plus' (Mult' (LitInt' 3) (Var "y")) (Minus' (Var "x") (LitInt' 10))))
      let f = V.fromList [parseArithmeticalExpr (LitInt' 2), parseArithmeticalExpr (LitInt' 3)]
      let c = parseArithmeticalExpr (LitInt' (-10))
      makeVectorFromArithmeticalExpr xs e `shouldBe` Just (f, c)
  describe "normalizeArithmeticalExpr" $ do
    it "works" $ do
      let e = Plus' (LitInt' 2) (Plus' (Var "a") (LitInt' (-2)))
      let expected = Var "a"
      (formatArithmeticalExpr . normalizeArithmeticalExpr . parseArithmeticalExpr) e `shouldBe` expected
