{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.EvaluateSpec (spec) where

import Jikka.Core.Evaluate (Token (..), Value (..), run')
import Jikka.Core.Language.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ToplevelLetRec
            "solve@0"
            [("xs@1", ListTy IntTy)]
            IntTy
            ( AppBuiltin
                Plus
                [ AppBuiltin Sum [Var "xs@1"],
                  AppBuiltin (Len IntTy) [Var "xs@1"]
                ]
            )
            (ResultExpr (Var "solve@0"))
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
            "fact@0"
            [("n@1", IntTy)]
            IntTy
            ( App
                ( AppBuiltin
                    (If (FunTy [] IntTy))
                    [ AppBuiltin (Equal IntTy) [Var "n@1", Lit0],
                      Lam [] Lit1,
                      Lam
                        []
                        ( AppBuiltin
                            Mult
                            [ Var "n@1",
                              App (Var "fact@0") [AppBuiltin Minus [Var "n@1", Lit1]]
                            ]
                        )
                    ]
                )
                []
            )
            (ResultExpr (Var "fact@0"))
    let tokens =
          [ Token "10"
          ]
    let expected = ValInt 3628800
    run' tokens prog `shouldBe` Right expected
