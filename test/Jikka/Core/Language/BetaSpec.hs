{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Language.BetaSpec
  ( spec,
  )
where

import Jikka.Core.Language.Beta
import Jikka.Core.Language.Expr
import Test.Hspec

spec :: Spec
spec = do
  describe "substitute" $ do
    it "renames scoped variables of lam if required" $ do
      let x = "x"
      let a = Var "y"
      let e = Lam1 "y" IntTy (AppBuiltin Plus [Var "x", Var "y"])
      let expected = Lam1 "@0" IntTy (AppBuiltin Plus [Var "y", Var "@0"])
      substitute x a e `shouldBe` expected
    it "renames scoped variables of let if required" $ do
      let x = "x"
      let a = Var "y"
      let e = Let "y" IntTy (Var "y") (AppBuiltin Plus [Var "x", Var "y"])
      let expected = Let "@0" IntTy (Var "y") (AppBuiltin Plus [Var "y", Var "@0"])
      substitute x a e `shouldBe` expected
  describe "substitute'" $ do
    it "avoids variables in the env" $ do
      let used = ["@0", "x", "y"]
      let x = "x"
      let a = Var "y"
      let e = Lam1 "y" IntTy (AppBuiltin Plus [Var "x", Var "y"])
      let expected = Lam1 "@1" IntTy (AppBuiltin Plus [Var "y", Var "@1"])
      substitute' used x a e `shouldBe` expected
