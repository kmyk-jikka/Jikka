{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.AlphaSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.Alpha (run)
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
                "x"
                IntTy
                Lit0
                ( Let
                    "x$0"
                    IntTy
                    (Plus' (Var "x") Lit1)
                    (Var "x$0")
                )
            )
    run' prog `shouldBe` Right expected
