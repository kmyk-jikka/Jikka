module Jikka.Converter.Python.ToCoreSpec (spec) where

import Data.Either (isLeft)
import Jikka.Converter.Python.ToCore (run)
import Jikka.Language.Common.Name
import qualified Jikka.Language.Core.Expr as Y
import qualified Jikka.Language.Python.Typed.Stdlib as X
import qualified Jikka.Language.Python.Typed.Type as X
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          X.Program
            [ X.FunDef
                (FunName "solve@0")
                [(VarName "x@1", X.ATyBool)]
                X.ATyBool
                [ X.Return (X.UnOp X.Not (X.Var (VarName "x@1")))
                ]
            ]
    let expected =
          Y.ToplevelLet
            Y.Rec
            (VarName "solve@0")
            [(VarName "x@1", Y.BoolTy)]
            Y.BoolTy
            (Y.appBuiltin Y.Not [Y.Var (VarName "x@1")])
            (Y.ResultExpr (Y.Var (VarName "solve@0")))
    run input `shouldBe` Right expected
  it "converts a simple loop" $ do
    let input =
          X.Program
            [ X.FunDef
                (FunName "solve@0")
                [(VarName "n@1", X.ATyNat)]
                X.ATyNat
                [ X.Declare (VarName "xs@2") (X.ATyList X.ATyNat) [X.Var (VarName "n@1")],
                  X.For
                    (VarName "i@3")
                    X.ATyNat
                    (X.UnOp X.Range1 (X.Var (VarName "n@1")))
                    [ X.Assign (VarName "xs@2") [X.Var (VarName "i@3")] (X.BinOp X.Mult (X.Var (VarName "i@3")) (X.Var (VarName "i@3")))
                    ],
                  X.Return (X.UnOp X.Sum (X.Var (VarName "xs@2")))
                ]
            ]
    let expected =
          Y.ToplevelLet
            Y.Rec
            (VarName "solve@0")
            [(VarName "n@1", Y.IntTy)]
            Y.IntTy
            ( Y.Let
                (VarName "xs@2")
                (Y.ListTy Y.IntTy)
                ( Y.appBuiltin
                    (Y.Tabulate Y.IntTy)
                    [ Y.Var (VarName "n@1"),
                      Y.Lam
                        [(VarName "i@3", Y.IntTy)]
                        (Y.appBuiltin Y.Mult [Y.Var (VarName "i@3"), Y.Var (VarName "i@3")])
                    ]
                )
                (Y.appBuiltin Y.Sum [Y.Var (VarName "xs@2")])
            )
            (Y.ResultExpr (Y.Var (VarName "solve@0")))
    run input `shouldBe` Right expected
