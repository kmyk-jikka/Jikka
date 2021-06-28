{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.EvaluateSpec (spec) where

import Jikka.RestrictedPython.Evaluate
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Value
import Jikka.RestrictedPython.Language.WithoutLoc
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
                  (eqExp IntTy (name "n") (constIntExp 0))
                  [Return (constIntExp 1)]
                  [Return (binOp (name "n") Mult (call (name "fact") [binOp (name "n") Sub (constIntExp 1)]))]
              ]
          ]
    let e = call (name "fact") [constIntExp 10]
    let expected = IntVal 3628800
    run prog e `shouldBe` Right expected
  it "works with for-loop and assignment" $ do
    let prog =
          [ ToplevelFunctionDef
              "solve"
              [("n", IntTy)]
              IntTy
              [ AnnAssign (nameTrg "a") (ListTy IntTy) (call (constBuiltinExp (BuiltinList IntTy)) [call (constBuiltinExp BuiltinRange1) [name "n"]]),
                For
                  (tupleTrg [nameTrg "i", nameTrg "a_i"])
                  (call (constBuiltinExp (BuiltinEnumerate IntTy)) [name "a"])
                  [ AugAssign (subscriptTrg (nameTrg "a") (name "i")) Mult (name "a_i")
                  ],
                Return (call (constBuiltinExp BuiltinSum) [name "a"])
              ]
          ]
    let e = call (name "solve") [constIntExp 100]
    let expected = IntVal 328350
    run prog e `shouldBe` Right expected
