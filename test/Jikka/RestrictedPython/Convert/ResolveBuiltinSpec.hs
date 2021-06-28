{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.ResolveBuiltinSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Convert.ResolveBuiltin (run)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util
import Jikka.RestrictedPython.Language.WithoutLoc
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          toplevelMainDef
            [ Return (call (name "max") [list IntTy [constIntExp 2, constIntExp 3]]),
              Return (call (name "max") [constIntExp 2, constIntExp 3]),
              Return (call (name "max") [constIntExp 2, constIntExp 3, constIntExp 4])
            ]
    let expected =
          toplevelMainDef
            [ Return (call (constBuiltinExp (BuiltinMax1 (VarTy "$0"))) [list IntTy [constIntExp 2, constIntExp 3]]),
              Return (call (constBuiltinExp (BuiltinMax (VarTy "$1") 2)) [constIntExp 2, constIntExp 3]),
              Return (call (constBuiltinExp (BuiltinMax (VarTy "$2") 3)) [constIntExp 2, constIntExp 3, constIntExp 4])
            ]
    run' prog `shouldBe` Right expected
