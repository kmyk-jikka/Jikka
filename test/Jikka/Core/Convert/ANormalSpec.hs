{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.ANormalSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.ANormal (run)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ResultExpr $
            Plus'
              (Let "x" IntTy Lit1 (Var "x"))
              (App (Lam "y" IntTy (Var "y")) Lit1)
    let expected =
          ResultExpr $
            Let "x" IntTy Lit1 $
              Let "$1" (Fun1STy IntTy) (Lam "y" IntTy (Var "y")) $
                Let "$0" (Fun1STy IntTy) (Var "$1") $
                  Let "$2" IntTy (App (Var "$0") Lit1) $
                    Plus' (Var "x") (Var "$2")
    run' prog `shouldBe` Right expected
