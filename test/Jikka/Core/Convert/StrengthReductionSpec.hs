{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.StrengthReductionSpec (spec) where

import Jikka.Core.Convert.StrengthReduction (run)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ResultExpr
            ( Lam1
                "n@0"
                IntTy
                ( Sum'
                    ( Tabulate'
                        IntTy
                        (Var "n@0")
                        (Lam1 "x@1" IntTy (Mult' (Lit (LitInt 100)) (Var "x@1")))
                    )
                )
            )
    let expected =
          ResultExpr
            ( Lam1
                "n@0"
                IntTy
                ( Mult'
                    (Lit (LitInt 100))
                    ( FloorDiv'
                        ( Mult'
                            (Var "n@0")
                            (Plus' (Var "n@0") (Negate' Lit1))
                        )
                        Lit2
                    )
                )
            )
    run prog `shouldBe` Right expected
