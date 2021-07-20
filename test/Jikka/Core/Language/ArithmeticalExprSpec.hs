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
      (formatArithmeticalExpr . parseArithmeticalExpr) e `shouldBe` expected
