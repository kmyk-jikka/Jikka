{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.TypeInferSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.TypeInfer (run)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ResultExpr
            ( Let
                "x"
                (VarTy "t1")
                Lit0
                ( Let
                    "y"
                    (VarTy "t2")
                    (Plus' (Var "x") Lit1)
                    (Var "y")
                )
            )
    let expected =
          ResultExpr
            ( Let
                "x"
                IntTy
                Lit0
                ( Let
                    "y"
                    IntTy
                    (Plus' (Var "x") Lit1)
                    (Var "y")
                )
            )
    run' prog `shouldBe` Right expected
  it "works on let-rec" $ do
    let prog =
          ToplevelLetRec
            "f"
            [("x", VarTy "t1")]
            (VarTy "t2")
            (Var "x")
            (ResultExpr (App (Var "f") [Lit0]))
    let expected =
          ToplevelLetRec
            "f"
            [("x", IntTy)]
            IntTy
            (Var "x")
            (ResultExpr (App (Var "f") [Lit0]))
    run' prog `shouldBe` Right expected
  it "replaces undetermined types with 0-tuples" $ do
    let prog =
          ToplevelLetRec
            "f"
            [("x", VarTy "t1")]
            (VarTy "t2")
            (Var "x")
            (ResultExpr Lit0)
    let expected =
          ToplevelLetRec
            "f"
            [("x", TupleTy [])]
            (TupleTy [])
            (Var "x")
            (ResultExpr Lit0)
    run' prog `shouldBe` Right expected
