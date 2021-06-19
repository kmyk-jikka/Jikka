{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.EvaluateSpec (spec) where

import Jikka.RestrictedPython.Evaluate
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Value
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works with recursion" $ do
    let prog =
          [ ToplevelFunctionDef
              "fact"
              [("n", IntTy)]
              IntTy
              [ If
                  (Compare (Name "n") Eq' (Constant (ConstInt 0)))
                  [Return (Constant (ConstInt 1))]
                  [Return (BinOp (Name "n") Mult (Call (Name "fact") [BinOp (Name "n") Sub (Constant (ConstInt 1))]))]
              ]
          ]
    let e = Call (Name "fact") [Constant (ConstInt 10)]
    let expected = IntVal 3628800
    run prog e `shouldBe` Right expected
  it "works with for-loop and assignment" $ do
    let prog =
          [ ToplevelFunctionDef
              "solve"
              [("n", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "a") (ListTy IntTy) (Call (Name "list") [Call (Name "range") [Name "n"]]),
                For
                  (TupleTrg [NameTrg "i", NameTrg "a_i"])
                  (Call (Name "enumerate") [Name "a"])
                  [ AugAssign (SubscriptTrg (NameTrg "a") (Name "i")) Mult (Name "a_i")
                  ],
                Return (Call (Name "sum") [Name "a"])
              ]
          ]
    let e = Call (Name "solve") [Constant (ConstInt 100)]
    let expected = IntVal 328350
    run prog e `shouldBe` Right expected
