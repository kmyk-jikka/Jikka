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
import Jikka.Core.Language.Util
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let ts = [IntTy, IntTy]
    let prog =
          ResultExpr $
            App (Lam "x" (TupleTy ts) (Plus' (Proj' ts 0 (Var "x")) (Proj' ts 1 (Var "x")))) (uncurryApp (Tuple' ts) [LitInt' 0, LitInt' 1])
    let expected =
          ResultExpr $
            App2 (Lam2 "x$0" IntTy "x$1" IntTy (Plus' (Var "x$0") (Var "x$1"))) (LitInt' 0) (LitInt' 1)
    run' prog `shouldBe` Right expected
  it "works on foldl" $ do
    let prog =
          ResultExpr $
            Foldl' IntTy (TupleTy [IntTy]) (Lam2 "x" (TupleTy [IntTy]) "y" IntTy (uncurryApp (Tuple' [IntTy]) [Plus' (Proj' [IntTy] 0 (Var "x")) (Var "y")])) (uncurryApp (Tuple' [IntTy]) [LitInt' 0]) (Range1' (LitInt' 10))
    let expected =
          ResultExpr $
            uncurryApp (Tuple' [IntTy]) [Foldl' IntTy IntTy (Lam2 "x" IntTy "y" IntTy (Plus' (Var "x") (Var "y"))) (LitInt' 0) (Range1' (LitInt' 10))]
    run' prog `shouldBe` Right expected
