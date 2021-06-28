{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Language.VariableAnalysisSpec
  ( spec,
  )
where

import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.VariableAnalysis
import Jikka.RestrictedPython.Language.WithoutLoc
import Test.Hspec

spec :: Spec
spec = do
  describe "analyzeStatementMax" $ do
    it "works" $ do
      let e = AnnAssign (nameTrg "y") IntTy (name "x")
      let expected = (ReadList ["x"], WriteList ["y"])
      analyzeStatementMax e `shouldBe` expected
    it "works'" $ do
      let e = If (constBoolExp True) [AnnAssign (nameTrg "y") IntTy (name "x")] []
      let expected = (ReadList ["x"], WriteList ["y"])
      analyzeStatementMax e `shouldBe` expected
  describe "analyzeStatementMin" $ do
    it "works" $ do
      let e = AnnAssign (nameTrg "y") IntTy (name "x")
      let expected = (ReadList ["x"], WriteList ["y"])
      analyzeStatementMin e `shouldBe` expected
    it "works'" $ do
      let e = If (constBoolExp True) [AnnAssign (nameTrg "y") IntTy (name "x")] []
      let expected = (ReadList [], WriteList [])
      analyzeStatementMin e `shouldBe` expected
