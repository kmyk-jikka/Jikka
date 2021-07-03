{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.MakeEagerSpec (spec) where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.MakeEager (run)
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
            ( If'
                IntTy
                LitTrue
                Lit0
                Lit1
            )
    let expected =
          ResultExpr
            ( App
                ( If'
                    (FunTy (TupleTy []) IntTy)
                    LitTrue
                    (Lam "$0" (TupleTy []) Lit0)
                    (Lam "$1" (TupleTy []) Lit1)
                )
                (Tuple' [])
            )
    run' prog `shouldBe` Right expected
