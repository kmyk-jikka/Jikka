{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.RemoveUnbalancedIfSpec
  ( spec,
  )
where

import Jikka.RestrictedPython.Convert.RemoveUnbalancedIf (run)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          toplevelMainDef
            [ If
                (constBoolExp True)
                [ Return (constIntExp 0)
                ]
                [ AnnAssign (NameTrg "a") IntTy (constIntExp 0)
                ],
              AnnAssign (NameTrg "b") IntTy (constIntExp 1),
              Return (constIntExp 2)
            ]
    let expected =
          toplevelMainDef
            [ If
                (constBoolExp True)
                [ Return (constIntExp 0)
                ]
                [ AnnAssign (NameTrg "a") IntTy (constIntExp 0),
                  AnnAssign (NameTrg "b") IntTy (constIntExp 1),
                  Return (constIntExp 2)
                ]
            ]
    run prog `shouldBe` expected
