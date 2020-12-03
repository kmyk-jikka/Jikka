module Jikka.Converter.Core.StrengthReductionSpec (spec) where

import Jikka.Converter.Core.StrengthReduction (run)
import Jikka.Language.Common.Name
import Jikka.Language.Core.BuiltinPatterns
import Jikka.Language.Core.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          ResultExpr
            ( Lam1
                (VarName "n@0")
                IntTy
                ( Sum'
                    ( Tabulate'
                        IntTy
                        (Var (VarName "n@0"))
                        (Lam1 (VarName "x@1") IntTy (Mult' (Lit (LitInt 100)) (Var (VarName "x@1"))))
                    )
                )
            )
    let expected =
          ResultExpr
            ( Lam1
                (VarName "n@0")
                IntTy
                ( Mult'
                    (Lit (LitInt 100))
                    ( FloorDiv'
                        ( Mult'
                            (Var (VarName "n@0"))
                            (Plus' (Var (VarName "n@0")) (Negate' Lit1))
                        )
                        Lit2
                    )
                )
            )
    run input `shouldBe` Right expected
