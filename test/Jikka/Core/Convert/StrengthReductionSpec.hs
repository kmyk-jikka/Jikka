module Jikka.Core.Convert.StrengthReductionSpec (spec) where

import Jikka.Common.Language.Name
import Jikka.Core.Convert.StrengthReduction (run)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
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
