module Jikka.Core.Convert.RemoveUnusedVarsSpec (spec) where

import Jikka.Core.Convert.RemoveUnusedVars (run)
import Jikka.Core.Language.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          ToplevelLetRec
            (VarName "solve@0")
            [(VarName "x@1", BoolTy)]
            BoolTy
            (Let (VarName "y@2") IntTy Lit0 (Var (VarName "x@1")))
            (ResultExpr (Var (VarName "solve@0")))
    let expected =
          ToplevelLet
            (VarName "solve@0")
            (FunTy [BoolTy] BoolTy)
            (Lam [(VarName "x@1", BoolTy)] (Var (VarName "x@1")))
            (ResultExpr (Var (VarName "solve@0")))
    run input `shouldBe` Right expected
