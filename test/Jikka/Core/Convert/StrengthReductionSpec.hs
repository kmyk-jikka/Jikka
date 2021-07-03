{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.StrengthReductionSpec (spec) where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.StrengthReduction (run)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ResultExpr
            ( Lam
                "n"
                IntTy
                ( Sum'
                    ( Tabulate'
                        IntTy
                        (Var "n")
                        (Lam "x" IntTy (Mult' (Lit (LitInt 100)) (Var "x")))
                    )
                )
            )
    let expected =
          ResultExpr
            ( Lam
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
    run' prog `shouldBe` Right expected
