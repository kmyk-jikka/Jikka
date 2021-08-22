{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Language.ArithmeticExprSpec
  ( spec,
  )
where

import qualified Data.Vector as V
import Jikka.Core.Language.ArithmeticExpr
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

spec :: Spec
spec = do
  describe "makeVectorFromArithmeticExpr" $ do
    it "works" $ do
      let xs = V.fromList ["x", "y"]
      let e =
            parseArithmeticExpr
              (Plus' (Var "x") (Plus' (Mult' (LitInt' 3) (Var "y")) (Minus' (Var "x") (LitInt' 10))))
      let f = V.fromList [parseArithmeticExpr (LitInt' 2), parseArithmeticExpr (LitInt' 3)]
      let c = parseArithmeticExpr (LitInt' (-10))
      makeVectorFromArithmeticExpr xs e `shouldBe` Just (f, c)
    it "fails with modulo" $ do
      -- See https://github.com/kmyk/Jikka/issues/173
      let xs = V.singleton "x"
      let e = parseArithmeticExpr (FloorMod' (Var "x") (LitInt' 3))
      makeVectorFromArithmeticExpr xs e `shouldBe` Nothing
  describe "normalizeArithmeticExpr" $ do
    it "works" $ do
      let e = Plus' (LitInt' 2) (Plus' (Var "a") (LitInt' (-2)))
      let expected = Var "a"
      (formatArithmeticExpr . parseArithmeticExpr) e `shouldBe` expected
