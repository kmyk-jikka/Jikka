{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.TrivialLetEliminationSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.TrivialLetElimination (run)
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
                "f"
                (FunTy [IntTy] IntTy)
                (Lam [("y", IntTy)] (Var "y"))
                ( Let
                    "x"
                    IntTy
                    Lit1
                    (App (Var "f") [Plus' (Var "x") (Var "x")])
                )
            )
    let expected =
          ResultExpr
            ( Let
                "x"
                IntTy
                Lit1
                (App (Lam [("y", IntTy)] (Var "y")) [Plus' (Var "x") (Var "x")])
            )
    run' prog `shouldBe` Right expected
