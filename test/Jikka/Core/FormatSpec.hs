module Jikka.Core.FormatSpec
  ( spec,
  )
where

import Jikka.Core.Format
import Jikka.Core.Language.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let program =
          ToplevelLet
            Rec
            (VarName "solve@0")
            [(VarName "n@1", IntTy)]
            IntTy
            ( Let
                (VarName "xs@2")
                (ListTy IntTy)
                ( AppBuiltin
                    (Tabulate IntTy)
                    [ Var (VarName "n@1"),
                      Lam
                        [(VarName "i@3", IntTy)]
                        (AppBuiltin Mult [Var (VarName "i@3"), Var (VarName "i@3")])
                    ]
                )
                (AppBuiltin Sum [Var (VarName "xs@2")])
            )
            (ResultExpr (Var (VarName "solve@0")))
    let expected =
          unlines
            [ "let rec solve@0 (n@1: int): int =",
              "    let xs@2: int list =",
              "        tabulate(n@1, (fun (i@3: int) ->",
              "            (i@3 * i@3)",
              "        ))",
              "    in sum(xs@2)",
              "in",
              "solve@0"
            ]
    run' program `shouldBe` expected
