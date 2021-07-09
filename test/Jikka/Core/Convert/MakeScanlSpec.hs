{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.MakeScanlSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.Beta as Beta
import Jikka.Core.Convert.MakeScanl (rule)
import qualified Jikka.Core.Convert.ShortCutFusion as ShortCutFusion
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.RewriteRules
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . applyRewriteRuleProgram' (rule <> ShortCutFusion.rule <> Beta.rule)

spec :: Spec
spec = describe "run" $ do
  it "works on a[i + 1] = f(a[i])" $ do
    let prog =
          ResultExpr $
            Foldl'
              IntTy
              (ListTy IntTy)
              ( Lam2
                  "a"
                  (ListTy IntTy)
                  "i"
                  IntTy
                  (SetAt' IntTy (Var "a") (Plus' (Var "i") Lit1) (Mult' Lit2 (At' IntTy (Var "a") (Var "i"))))
              )
              (SetAt' IntTy (Range1' (LitInt' 100)) (LitInt' 0) (LitInt' 1))
              (Range1' (LitInt' 99))
    let expected =
          ResultExpr $
            Scanl'
              IntTy
              IntTy
              ( Lam2
                  "a$0"
                  IntTy
                  "i"
                  IntTy
                  (Mult' Lit2 (Var "a$0"))
              )
              (LitInt' 1)
              (Range1' (LitInt' 99))
    run' prog `shouldBe` Right expected

  it "works on a[i] = f(a[i - 1])" $ do
    let prog =
          ResultExpr $
            Foldl'
              IntTy
              (ListTy IntTy)
              ( Lam2
                  "a"
                  (ListTy IntTy)
                  "i"
                  IntTy
                  (SetAt' IntTy (Var "a") (Var "i") (Mult' Lit2 (At' IntTy (Var "a") (Minus' (Var "i") Lit1))))
              )
              (SetAt' IntTy (Range1' (LitInt' 100)) (LitInt' 0) (LitInt' 1))
              (Range2' (LitInt' 1) (LitInt' 99))
    let expected =
          ResultExpr $
            Scanl'
              IntTy
              IntTy
              ( Lam2
                  "$3"
                  IntTy
                  "$2"
                  IntTy
                  (Mult' Lit2 (Var "$3"))
              )
              (LitInt' 1)
              (Range1' (Minus' (LitInt' 99) (LitInt' 1)))
    run' prog `shouldBe` Right expected
