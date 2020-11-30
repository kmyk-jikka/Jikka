module Jikka.Converter.Core.CleanUpSpec (spec) where

import Jikka.Converter.Core.CleanUp (run)
import Jikka.Language.Common.Name
import Jikka.Language.Core.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          ToplevelLet
            Rec
            (VarName "solve@0")
            [(VarName "x@1", BoolTy)]
            BoolTy
            (AppBuiltin Not [AppBuiltin Not [Var (VarName "x@1")]])
            (ResultExpr (Var (VarName "solve@0")))
    let expected =
          ToplevelLet
            NonRec
            (VarName "solve@0")
            [(VarName "x@1", BoolTy)]
            BoolTy
            (Var (VarName "x@1"))
            (ResultExpr (Var (VarName "solve@0")))
    run input `shouldBe` Right expected
