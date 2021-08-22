{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.CloseSumSpec (spec) where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.CloseSum (rule)
import qualified Jikka.Core.Convert.ConstantFolding as ConstantFolding
import qualified Jikka.Core.Convert.ShortCutFusion as ShortCutFusion
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.RewriteRules
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . applyRewriteRuleProgram' (rule <> ConstantFolding.rule <> ShortCutFusion.rule)

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ResultExpr
            ( Lam
                "n"
                IntTy
                ( Sum'
                    ( Map'
                        IntTy
                        IntTy
                        (Lam "x" IntTy (Mult' (Lit (LitInt 100)) (Var "x")))
                        (Range1' (Var "n"))
                    )
                )
            )
    let expected =
          ResultExpr
            ( Lam
                "n"
                IntTy
                ( Mult'
                    (Lit (LitInt 100))
                    ( JustDiv'
                        ( Mult'
                            (Var "n")
                            (Minus' (Var "n") Lit1)
                        )
                        Lit2
                    )
                )
            )
    run' prog `shouldBe` Right expected
