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
