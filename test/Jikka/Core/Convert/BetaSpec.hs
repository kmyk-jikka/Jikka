{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.BetaSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.Beta (run)
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
                "a"
                IntTy
                ( App
                    (Lam "x" IntTy (Plus' (Var "x") (Var "x")))
                    (Var "a")
                )
            )
    let expected =
          ResultExpr
            ( Lam
                "a$0"
                IntTy
                (Plus' (Var "a$0") (Var "a$0"))
            )
    run' prog `shouldBe` Right expected
