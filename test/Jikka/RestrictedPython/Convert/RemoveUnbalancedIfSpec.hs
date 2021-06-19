{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.RemoveUnbalancedIfSpec
  ( spec,
  )
where

import Jikka.RestrictedPython.Convert.RemoveUnbalancedIf (run)
import Jikka.RestrictedPython.Language.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ If
                  (Constant (ConstBool True))
                  [ Return (Constant (ConstInt 0))
                  ]
                  [ AnnAssign (NameTrg "a") IntTy (Constant (ConstInt 0))
                  ],
                AnnAssign (NameTrg "b") IntTy (Constant (ConstInt 1)),
                Return (Constant (ConstInt 2))
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ If
                  (Constant (ConstBool True))
                  [ Return (Constant (ConstInt 0))
                  ]
                  [ AnnAssign (NameTrg "a") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "b") IntTy (Constant (ConstInt 1)),
                    Return (Constant (ConstInt 2))
                  ]
              ]
          ]
    run prog `shouldBe` expected
