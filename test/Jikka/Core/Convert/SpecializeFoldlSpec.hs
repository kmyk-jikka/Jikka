{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.SpecializeFoldlSpec (spec) where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.SpecializeFoldl (run)
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
                ( Foldl'
                    IntTy
                    IntTy
                    ( Lam2
                        "y"
                        IntTy
                        "x"
                        IntTy
                        (Plus' (Var "y") (Var "x"))
                    )
                    Lit0
                    (Range1' (Var "n"))
                )
            )
    let expected =
          ResultExpr
            ( Lam
                "n"
                IntTy
                ( Sum'
                    ( Cons'
                        IntTy
                        Lit0
                        ( Map'
                            IntTy
                            IntTy
                            (Lam "x" IntTy (Var "x"))
                            (Range1' (Var "n"))
                        )
                    )
                )
            )
    run' prog `shouldBe` Right expected
