{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Language.UtilSpec
  ( spec,
  )
where

import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util
import Jikka.RestrictedPython.Language.WithoutLoc
import Test.Hspec

spec :: Spec
spec = describe "doesAlwaysReturn" $ do
  it "works" $ do
    let stmt = AnnAssign (nameTrg "a") IntTy (constIntExp 0)
    let expected = False
    doesAlwaysReturn stmt `shouldBe` expected
  it "works'" $ do
    let stmt = Return (name "a")
    let expected = True
    doesAlwaysReturn stmt `shouldBe` expected
  it "returns true for an if-statement which both branches always return" $ do
    let stmt =
          If
            (constBoolExp True)
            [ AnnAssign (nameTrg "b") IntTy (constIntExp 0),
              Return (name "a"),
              AugAssign (nameTrg "b") Add (name "1")
            ]
            [ Return (constIntExp 1)
            ]
    let expected = True
    doesAlwaysReturn stmt `shouldBe` expected
  it "returns false for for-statement" $ do
    let stmt = For (nameTrg "x") (list IntTy []) [Return (constIntExp 0)]
    let expected = False
    doesAlwaysReturn stmt `shouldBe` expected
