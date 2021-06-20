{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.EvaluateSpec (spec) where

import Jikka.RestrictedPython.Evaluate
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util
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
                  (Compare (Name "n") (CmpOp' Eq' IntTy) (constIntExp 0))
                  [Return (constIntExp 1)]
                  [Return (BinOp (Name "n") Mult (Call (Name "fact") [BinOp (Name "n") Sub (constIntExp 1)]))]
              ]
          ]
    let e = Call (Name "fact") [constIntExp 10]
    let expected = IntVal 3628800
    run prog e `shouldBe` Right expected
  it "works with for-loop and assignment" $ do
    let prog =
          [ ToplevelFunctionDef
              "solve"
              [("n", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "a") (ListTy IntTy) (Call (constBuiltinExp (BuiltinList IntTy)) [Call (constBuiltinExp BuiltinRange1) [Name "n"]]),
                For
                  (TupleTrg [NameTrg "i", NameTrg "a_i"])
                  (Call (constBuiltinExp (BuiltinEnumerate IntTy)) [Name "a"])
                  [ AugAssign (SubscriptTrg (NameTrg "a") (Name "i")) Mult (Name "a_i")
                  ],
                Return (Call (constBuiltinExp BuiltinSum) [Name "a"])
              ]
          ]
    let e = Call (Name "solve") [constIntExp 100]
    let expected = IntVal 328350
    run prog e `shouldBe` Right expected
