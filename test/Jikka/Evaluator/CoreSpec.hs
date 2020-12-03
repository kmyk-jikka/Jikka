module Jikka.Evaluator.CoreSpec (spec) where

import Jikka.Evaluator.Core (Token (..), Value (..), run')
import Jikka.Language.Common.Name
import Jikka.Language.Core.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ToplevelLet
            Rec
            (VarName "solve@0")
            [(VarName "xs@1", ListTy IntTy)]
            IntTy
            ( AppBuiltin
                Plus
                [ AppBuiltin Sum [Var (VarName "xs@1")],
                  AppBuiltin (Len IntTy) [Var (VarName "xs@1")]
                ]
            )
            (ResultExpr (Var (VarName "solve@0")))
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
          ToplevelLet
            Rec
            (VarName "fact@0")
            [(VarName "n@1", IntTy)]
            IntTy
            ( App
                ( AppBuiltin
                    (If (FunTy [] IntTy))
                    [ AppBuiltin (Equal IntTy) [Var (VarName "n@1"), Lit0],
                      Lam [] Lit1,
                      Lam
                        []
                        ( AppBuiltin
                            Mult
                            [ Var (VarName "n@1"),
                              App (Var (VarName "fact@0")) [AppBuiltin Minus [Var (VarName "n@1"), Lit1]]
                            ]
                        )
                    ]
                )
                []
            )
            (ResultExpr (Var (VarName "fact@0")))
    let tokens =
          [ Token "10"
          ]
    let expected = ValInt 3628800
    run' tokens prog `shouldBe` Right expected
