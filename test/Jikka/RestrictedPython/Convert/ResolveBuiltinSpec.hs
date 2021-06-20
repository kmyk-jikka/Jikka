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
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          toplevelMainDef
            [ Return (Call (Name "max") [List IntTy [constIntExp 2, constIntExp 3]]),
              Return (Call (Name "max") [constIntExp 2, constIntExp 3]),
              Return (Call (Name "max") [constIntExp 2, constIntExp 3, constIntExp 4])
            ]
    let expected =
          toplevelMainDef
            [ Return (Call (constBuiltinExp (BuiltinMax1 (VarTy "$0"))) [List IntTy [constIntExp 2, constIntExp 3]]),
              Return (Call (constBuiltinExp (BuiltinMax (VarTy "$1") 2)) [constIntExp 2, constIntExp 3]),
              Return (Call (constBuiltinExp (BuiltinMax (VarTy "$2") 3)) [constIntExp 2, constIntExp 3, constIntExp 4])
            ]
    run' prog `shouldBe` Right expected
