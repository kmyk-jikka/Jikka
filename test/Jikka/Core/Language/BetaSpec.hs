{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Language.BetaSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Core.Language.Beta
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

substitute' :: VarName -> Expr -> Expr -> Expr
substitute' x e = flip evalAlpha 0 . substitute x e

spec :: Spec
spec = do
  describe "substitute" $ do
    it "renames scoped variables of lam if required" $ do
      let x = "x"
      let a = Var "y"
      let e = Lam "y" IntTy (Plus' (Var "x") (Var "y"))
      let expected = Lam "y$0" IntTy (Plus' (Var "y") (Var "y$0"))
      substitute' x a e `shouldBe` expected
    it "renames scoped variables of let if required" $ do
      let x = "x"
      let a = Var "y"
      let e = Let "y" IntTy (Var "y") (Plus' (Var "x") (Var "y"))
      let expected = Let "y$0" IntTy (Var "y") (Plus' (Var "y") (Var "y$0"))
      substitute' x a e `shouldBe` expected
