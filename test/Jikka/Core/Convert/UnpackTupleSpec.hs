{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.UnpackTupleSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.UnpackTuple (run)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let ts = [IntTy, IntTy]
    let prog =
          ResultExpr $
            App (Lam [("x", TupleTy ts)] (Plus' (Proj' ts 0 (Var "x")) (Proj' ts 1 (Var "x")))) [Tuple' ts [LitInt' 0, LitInt' 1]]
    let expected =
          ResultExpr $
            App (Lam [("x$1", IntTy), ("x$2", IntTy)] (Plus' (Var "x$1") (Var "x$2"))) [LitInt' 0, LitInt' 1]
    run' prog `shouldBe` Right expected
