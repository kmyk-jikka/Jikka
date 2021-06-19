{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.SplitLoopsSpec
  ( spec,
  )
where

import Jikka.RestrictedPython.Convert.SplitLoops (run')
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
                AnnAssign (NameTrg "b") IntTy (Constant (ConstInt 0)),
                For
                  (NameTrg "i")
                  (Call (Name "range") [Constant (ConstInt 10)])
                  [ AnnAssign (NameTrg "c") IntTy (Name "b"),
                    AugAssign (NameTrg "a") Add (Name "i"),
                    AugAssign (NameTrg "b") Add (Name "c")
                  ]
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ AnnAssign (NameTrg "a") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "b") IntTy (Constant (ConstInt 0)),
                For
                  (NameTrg "i")
                  (Call (Name "range") [Constant (ConstInt 10)])
                  [ AnnAssign (NameTrg "c") IntTy (Name "b"),
                    AugAssign (NameTrg "b") Add (Name "c")
                  ],
                For
                  (NameTrg "i")
                  (Call (Name "range") [Constant (ConstInt 10)])
                  [ AugAssign (NameTrg "a") Add (Name "i")
                  ]
              ]
          ]
    run' prog `shouldBe` expected
