{-# LANGUAGE OverloadedStrings #-}

module Jikka.Converter.Core.ValueAppsSpec
  ( spec,
  )
where

import Jikka.Converter.Core.ValueApps (run)
import Jikka.Language.Core.BuiltinPatterns
import Jikka.Language.Core.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          ResultExpr $
            Plus'
              (Let "x" IntTy Lit1 (Var "x"))
              (App (Lam1 "x" IntTy (Var "x")) [Lit1])
    let expected =
          ResultExpr
            $ Let "x@0" IntTy Lit1
            $ Let "@3" (Fun1Ty IntTy) (Lam1 "x@1" IntTy (Var "x@1"))
            $ Let "@2" (Fun1Ty IntTy) (Var "@3")
            $ Let "@4" IntTy (App (Var "@2") [Lit1])
            $ Plus' (Var "x@0") (Var "@4")
    run input `shouldBe` Right expected
