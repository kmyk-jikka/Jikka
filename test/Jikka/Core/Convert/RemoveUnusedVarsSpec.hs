module Jikka.Core.Convert.RemoveUnusedVarsSpec (spec) where

import Jikka.Core.Convert.RemoveUnusedVars (run)
import Jikka.Core.Language.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          ToplevelLetRec
            (VarName "solve")
            [(VarName "x", BoolTy)]
            BoolTy
            (Let (VarName "y") IntTy Lit0 (Var (VarName "x")))
            (ResultExpr (Var (VarName "solve")))
    let expected =
          ToplevelLet
            (VarName "solve")
            (FunTy [BoolTy] BoolTy)
            (Lam [(VarName "x", BoolTy)] (Var (VarName "x")))
            (ResultExpr (Var (VarName "solve")))
    run input `shouldBe` Right expected
