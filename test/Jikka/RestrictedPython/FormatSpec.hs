{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.FormatSpec
  ( spec,
  )
where

import Jikka.RestrictedPython.Format
import Jikka.RestrictedPython.Language.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let program =
          [ ToplevelFunctionDef
              "solve$0"
              [("x$1", IntTy)]
              (VarTy "t$1")
              [ Return (UnaryOp USub (Name "x$1"))
              ]
          ]
    let formatted =
          unlines
            [ "def solve$0(x$1: int) -> t$1:",
              "    return - x$1"
            ]
    run' program `shouldBe` formatted
