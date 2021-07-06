{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.PropagateModSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.PropagateMod (run)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let m = LitInt' 1000000007
    let f e = FloorMod' e m
    let prog =
          ResultExpr
            ( Lam
                "y"
                IntTy
                (f (App (Lam "x" IntTy (Plus' (Mult' (Var "x") (Var "x")) (Var "x"))) (Var "y")))
            )
    let expected =
          ResultExpr
            ( Lam
                "y"
                IntTy
                (App (Lam "x$0" IntTy (ModPlus' (ModMult' (f (Var "x$0")) (f (Var "x$0")) m) (f (Var "x$0")) m)) (Var "y"))
            )
    run' prog `shouldBe` Right expected
