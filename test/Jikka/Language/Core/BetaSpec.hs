module Jikka.Language.Core.BetaSpec (spec) where

import Jikka.Language.Common.Name
import Jikka.Language.Core.Beta
import Jikka.Language.Core.Expr
import Test.Hspec

spec :: Spec
spec = do
  describe "substitute" $ do
    it "renames scoped variables of lam if required" $ do
      let x = VarName "x"
      let a = Var (VarName "y")
      let e = Lam1 (VarName "y") IntTy (AppBuiltin Plus [Var (VarName "x"), Var (VarName "y")])
      let expected = Lam1 (VarName "a@0") IntTy (AppBuiltin Plus [Var (VarName "y"), Var (VarName "a@0")])
      substitute x a e `shouldBe` expected
    it "renames scoped variables of let if required" $ do
      let x = VarName "x"
      let a = Var (VarName "y")
      let e = Let (VarName "y") IntTy (Var (VarName "y")) (AppBuiltin Plus [Var (VarName "x"), Var (VarName "y")])
      let expected = Let (VarName "a@0") IntTy (Var (VarName "y")) (AppBuiltin Plus [Var (VarName "y"), Var (VarName "a@0")])
      substitute x a e `shouldBe` expected
  describe "substitute'" $ do
    it "avoids variables in the env" $ do
      let used = [VarName "a@0", VarName "x", VarName "y"]
      let x = VarName "x"
      let a = Var (VarName "y")
      let e = Lam1 (VarName "y") IntTy (AppBuiltin Plus [Var (VarName "x"), Var (VarName "y")])
      let expected = Lam1 (VarName "a@1") IntTy (AppBuiltin Plus [Var (VarName "y"), Var (VarName "a@1")])
      substitute' used x a e `shouldBe` expected
