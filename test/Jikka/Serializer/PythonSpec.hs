module Jikka.Serializer.PythonSpec
  ( spec,
  )
where

import Data.Text (unpack)
import Jikka.Language.Common.Name
import Jikka.Language.Python.Typed.Stdlib
import Jikka.Language.Python.Typed.Type
import Jikka.Serializer.Python
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
