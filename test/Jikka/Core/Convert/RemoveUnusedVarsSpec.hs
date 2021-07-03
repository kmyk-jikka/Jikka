{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.RemoveUnusedVarsSpec (spec) where

import Jikka.Core.Convert.RemoveUnusedVars (run)
import Jikka.Core.Language.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ToplevelLetRec
            "solve"
            [("x", BoolTy)]
            BoolTy
            (Let "y" IntTy Lit0 (Var "x"))
            (ResultExpr (Var "solve"))
    let expected =
          ToplevelLet
            "solve"
            (FunTy [BoolTy] BoolTy)
            (Lam [("x", BoolTy)] (Var "x"))
            (ResultExpr (Var "solve"))
    run prog `shouldBe` Right expected
