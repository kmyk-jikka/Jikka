{-# LANGUAGE OverloadedStrings #-}

module Jikka.Converter.Core.AlphaSpec
  ( spec,
  )
where

import Jikka.Converter.Core.Alpha (run)
import Jikka.Language.Core.BuiltinPatterns
import Jikka.Language.Core.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          ResultExpr
            ( Let
                "x"
                IntTy
                Lit0
                ( Let
                    "x"
                    IntTy
                    (Plus' (Var "x") Lit1)
                    (Var "x")
                )
            )
    let expected =
          ResultExpr
            ( Let
                "x@0"
                IntTy
                Lit0
                ( Let
                    "x@1"
                    IntTy
                    (Plus' (Var "x@0") Lit1)
                    (Var "x@1")
                )
            )
    run input `shouldBe` Right expected
