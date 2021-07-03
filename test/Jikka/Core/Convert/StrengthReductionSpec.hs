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
                "n"
                IntTy
                ( Sum'
                    ( Tabulate'
                        IntTy
                        (Var "n")
                        (Lam1 "x" IntTy (Mult' (Lit (LitInt 100)) (Var "x")))
                    )
                )
            )
    let expected =
          ResultExpr
            ( Lam1
                "n"
                IntTy
                ( Mult'
                    (Lit (LitInt 100))
                    ( FloorDiv'
                        ( Mult'
                            (Var "n")
                            (Plus' (Var "n") (Negate' Lit1))
                        )
                        Lit2
                    )
                )
            )
    run prog `shouldBe` Right expected
