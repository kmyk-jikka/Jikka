module Jikka.RestrictedPython.FormatSpec
  ( spec,
  )
where

import Jikka.Common.Language.Name
import Jikka.RestrictedPython.Format
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Stdlib
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let program =
          Program
            [ FunDef
                (FunName "solve@0")
                [(VarName "x@1", ATyInt)]
                (ATyVar (TypeName "t@1"))
                [ Return (UnOp Negate (Var (VarName "x@1")))
                ]
            ]
    let formatted =
          unlines
            [ "def solve@0(x@1: int) -> t@1:",
              "    return - (x@1)"
            ]
    run' program `shouldBe` formatted
