{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.RemoveUnreachableSpec
  ( spec,
  )
where

import Jikka.RestrictedPython.Convert.RemoveUnreachable (run)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util
import Jikka.RestrictedPython.Language.WithoutLoc
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          toplevelMainDef
            [ AnnAssign (nameTrg "a") IntTy (constIntExp 0),
              If
                (constBoolExp True)
                [ AnnAssign (nameTrg "b") IntTy (constIntExp 0),
                  Return (name "a"),
                  AugAssign (nameTrg "b") Add (name "1")
                ]
                [ Return (constIntExp 1)
                ],
              AugAssign (nameTrg "a") Add (constIntExp 1)
            ]
    let expected =
          toplevelMainDef
            [ AnnAssign (nameTrg "a") IntTy (constIntExp 0),
              If
                (constBoolExp True)
                [ AnnAssign (nameTrg "b") IntTy (constIntExp 0),
                  Return (name "a")
                ]
                [ Return (constIntExp 1)
                ]
            ]
    run prog `shouldBe` expected
