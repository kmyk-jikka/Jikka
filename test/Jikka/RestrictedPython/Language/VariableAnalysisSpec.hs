{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Language.VariableAnalysisSpec
  ( spec,
  )
where

import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.VariableAnalysis
import Test.Hspec

spec :: Spec
spec = do
  describe "analyzeStatementMax" $ do
    it "works" $ do
      let e = AnnAssign (NameTrg "y") IntTy (Name "x")
      let expected = (ReadList ["x"], WriteList ["y"])
      analyzeStatementMax e `shouldBe` expected
    it "works'" $ do
      let e = If (Constant (ConstBool True)) [AnnAssign (NameTrg "y") IntTy (Name "x")] []
      let expected = (ReadList ["x"], WriteList ["y"])
      analyzeStatementMax e `shouldBe` expected
  describe "analyzeStatementMin" $ do
    it "works" $ do
      let e = AnnAssign (NameTrg "y") IntTy (Name "x")
      let expected = (ReadList ["x"], WriteList ["y"])
      analyzeStatementMin e `shouldBe` expected
    it "works'" $ do
      let e = If (Constant (ConstBool True)) [AnnAssign (NameTrg "y") IntTy (Name "x")] []
      let expected = (ReadList [], WriteList [])
      analyzeStatementMin e `shouldBe` expected
