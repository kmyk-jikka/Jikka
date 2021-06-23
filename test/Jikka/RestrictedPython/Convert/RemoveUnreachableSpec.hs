{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.RemoveUnreachableSpec
  ( spec,
  )
where

import Jikka.RestrictedPython.Convert.RemoveUnreachable (run)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          toplevelMainDef
            [ AnnAssign (NameTrg "a") IntTy (constIntExp 0),
              If
                (constBoolExp True)
                [ AnnAssign (NameTrg "b") IntTy (constIntExp 0),
                  Return (Name "a"),
                  AugAssign (NameTrg "b") Add (Name "1")
                ]
                [ Return (constIntExp 1)
                ],
              AugAssign (NameTrg "a") Add (constIntExp 1)
            ]
    let expected =
          toplevelMainDef
            [ AnnAssign (NameTrg "a") IntTy (constIntExp 0),
              If
                (constBoolExp True)
                [ AnnAssign (NameTrg "b") IntTy (constIntExp 0),
                  Return (Name "a")
                ]
                [ Return (constIntExp 1)
                ]
            ]
    run prog `shouldBe` expected
