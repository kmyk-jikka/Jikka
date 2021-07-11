{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.EtaSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.Eta (run)
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
                "plus"
                (FunTy IntTy (FunTy IntTy IntTy))
                (Lit (LitBuiltin Plus))
                (Var "plus")
            )
    let expected =
          ResultExpr
            ( Let
                "plus"
                (FunTy IntTy (FunTy IntTy IntTy))
                (Lam "$0" IntTy (Lam "$1" IntTy (App2 (Lit (LitBuiltin Plus)) (Var "$0") (Var "$1"))))
                (Var "plus")
            )
    run' prog `shouldBe` Right expected
