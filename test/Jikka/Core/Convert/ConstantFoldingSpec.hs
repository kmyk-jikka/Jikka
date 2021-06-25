{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.ConstantFoldingSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.ConstantFolding (run)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ResultExpr $
            Lam [("x", IntTy)] (Plus' (Mult' (LitInt' 3) (Var "x")) (Plus' (LitInt' 2) (LitInt' 1)))
    let expected =
          ResultExpr $
            Lam [("x", IntTy)] (Plus' (Mult' (LitInt' 3) (Var "x")) (LitInt' 3))
    run' prog `shouldBe` Right expected
