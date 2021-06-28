{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.SplitLoopsSpec
  ( spec,
  )
where

import Jikka.RestrictedPython.Convert.SplitLoops (run')
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util (toplevelMainDef)
import Jikka.RestrictedPython.Language.WithoutLoc
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          toplevelMainDef
            [ AnnAssign (nameTrg "a") IntTy (constIntExp 0),
              AnnAssign (nameTrg "b") IntTy (constIntExp 0),
              For
                (nameTrg "i")
                (call (name "range") [constIntExp 10])
                [ AnnAssign (nameTrg "c") IntTy (name "b"),
                  AugAssign (nameTrg "a") Add (name "i"),
                  AugAssign (nameTrg "b") Add (name "c")
                ]
            ]
    let expected =
          toplevelMainDef
            [ AnnAssign (nameTrg "a") IntTy (constIntExp 0),
              AnnAssign (nameTrg "b") IntTy (constIntExp 0),
              For
                (nameTrg "i")
                (call (name "range") [constIntExp 10])
                [ AnnAssign (nameTrg "c") IntTy (name "b"),
                  AugAssign (nameTrg "b") Add (name "c")
                ],
              For
                (nameTrg "i")
                (call (name "range") [constIntExp 10])
                [ AugAssign (nameTrg "a") Add (name "i")
                ]
            ]
    run' prog `shouldBe` expected
