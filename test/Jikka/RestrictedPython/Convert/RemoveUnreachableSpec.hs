{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.RemoveUnreachableSpec
  ( spec,
  )
where

import Jikka.RestrictedPython.Convert.RemoveUnreachable (run)
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
              [ AnnAssign (NameTrg "a") IntTy (Constant (ConstInt 0)),
                If
                  (Constant (ConstBool True))
                  [ AnnAssign (NameTrg "b") IntTy (Constant (ConstInt 0)),
                    Return (Name "a"),
                    AugAssign (NameTrg "b") Add (Name "1")
                  ]
                  [ Return (Constant (ConstInt 1))
                  ],
                AugAssign (NameTrg "a") Add (Constant (ConstInt 1))
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ AnnAssign (NameTrg "a") IntTy (Constant (ConstInt 0)),
                If
                  (Constant (ConstBool True))
                  [ AnnAssign (NameTrg "b") IntTy (Constant (ConstInt 0)),
                    Return (Name "a")
                  ]
                  [ Return (Constant (ConstInt 1))
                  ]
              ]
          ]
    run prog `shouldBe` expected
