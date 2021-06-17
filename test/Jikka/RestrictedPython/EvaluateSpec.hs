{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.EvaluateSpec (spec) where

import Jikka.RestrictedPython.Evaluate
import Jikka.RestrictedPython.Language.Expr
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
