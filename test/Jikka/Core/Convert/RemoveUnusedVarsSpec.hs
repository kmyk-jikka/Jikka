module Jikka.Core.Convert.RemoveUnusedVarsSpec (spec) where

import Jikka.Common.Language.Name
import Jikka.Core.Convert.RemoveUnusedVars (run)
import Jikka.Core.Language.Expr
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
            (Let (VarName "y@2") IntTy Lit0 (Var (VarName "x@1")))
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
