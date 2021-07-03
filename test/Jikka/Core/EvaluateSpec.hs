{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.EvaluateSpec (spec) where

import Jikka.Core.Evaluate (Token (..), Value (..), run')
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ToplevelLetRec
            "solve"
            [("xs", ListTy IntTy)]
            IntTy
            ( Plus'
                (Sum' (Var "xs"))
                (Len' IntTy (Var "xs"))
            )
            (ResultExpr (Var "solve"))
    let tokens =
          [ Token "3",
            Token "1",
            Token "2",
            Token "5"
          ]
    let expected = ValInt 11
    run' tokens prog `shouldBe` Right expected
  it "works on a recursive function" $ do
    let prog =
          ToplevelLetRec
            "fact"
            [("n", IntTy)]
            IntTy
            ( App
                ( If'
                    (FunTy [] IntTy)
                    (Equal' IntTy (Var "n") Lit0)
                    (Lam [] Lit1)
                    ( Lam
                        []
                        ( Mult'
                            (Var "n")
                            (App (Var "fact") [Minus' (Var "n") Lit1])
                        )
                    )
                )
                []
            )
            (ResultExpr (Var "fact"))
    let tokens =
          [ Token "10"
          ]
    let expected = ValInt 3628800
    run' tokens prog `shouldBe` Right expected
