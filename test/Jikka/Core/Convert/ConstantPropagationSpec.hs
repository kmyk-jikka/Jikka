{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.ConstantPropagationSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.ConstantPropagation (run)
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
                Lit1
                ( Let
                    "f"
                    (FunTy IntTy IntTy)
                    (Lam "y" IntTy (Var "y"))
                    (Plus' (Var "x") (Plus' (Var "x") (App (Var "f") (Var "x"))))
                )
            )
    let expected =
          ResultExpr
            ( Let
                "f"
                (FunTy IntTy IntTy)
                (Lam "y" IntTy (Var "y"))
                (Plus' Lit1 (Plus' Lit1 (App (Var "f") Lit1)))
            )
    run' prog `shouldBe` Right expected
