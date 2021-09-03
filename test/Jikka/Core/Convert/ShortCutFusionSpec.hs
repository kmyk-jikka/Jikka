{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.ShortCutFusionSpec (spec) where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.ShortCutFusion (run)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ResultExpr $
            Len' IntTy (Map' IntTy IntTy (Lam "n" IntTy (Mult' (LitInt' 2) (Var "n"))) (Range1' (LitInt' 100)))
    let expected =
          ResultExpr $
            LitInt' 100
    run' prog `shouldBe` Right expected
  it "squashes foldl-map combination" $ do
    let g = Lam2 "a$4" IntTy "i$5" IntTy (Plus' (Var "a$4") (Var "i$5"))
    let f = Lam "j$6" IntTy (Plus' (Var "j$6") Lit1)
    let prog =
          ResultExpr $
            Foldl' IntTy IntTy g Lit0 $
              Map' IntTy IntTy f $
                Range1' (LitInt' 100)
    let expected =
          ResultExpr $
            Foldl' IntTy IntTy (Lam2 "$0" IntTy "$1" IntTy (App2 g (Var "$0") (App f (Var "$1")))) Lit0 $
              Range1' (LitInt' 100)
    run' prog `shouldBe` Right expected
