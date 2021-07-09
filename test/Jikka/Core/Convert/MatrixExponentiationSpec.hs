{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.MatrixExponentiationSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.MatrixExponentiation (run)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Util
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works about matrices" $ do
    let ts2 = [IntTy, IntTy]
    let ts22 = [TupleTy ts2, TupleTy ts2]
    let proj i = Proj' ts2 i (Var "xs")
    let mkTuple ts = uncurryApp (Tuple' ts)
    let letConst = Let "c" IntTy (LitInt' 10)
    let k = LitInt' 100
    let base = mkTuple ts2 [LitInt' 12, LitInt' 34]
    let prog =
          ResultExpr
            ( letConst
                ( Iterate'
                    (TupleTy ts2)
                    k
                    ( Lam
                        "xs"
                        (TupleTy ts2)
                        (mkTuple ts2 [Plus' (proj 0) (Mult' (Var "c") (proj 1)), proj 0])
                    )
                    base
                )
            )
    let expected =
          ResultExpr
            ( letConst
                ( MatAp'
                    2
                    2
                    ( MatPow'
                        2
                        (mkTuple ts22 [mkTuple ts2 [Lit1, Var "c"], mkTuple ts2 [Lit1, Lit0]])
                        k
                    )
                    base
                )
            )
    run' prog `shouldBe` Right expected
  it "works about integers" $ do
    let letConst = Let "c" IntTy (LitInt' 10)
    let k = LitInt' 100
    let base = LitInt' 1234
    let prog =
          ResultExpr
            ( letConst
                ( Iterate'
                    IntTy
                    k
                    ( Lam
                        "x"
                        IntTy
                        (Plus' (Mult' (Var "c") (Var "x")) (LitInt' 2))
                    )
                    base
                )
            )
    let expected =
          ResultExpr
            ( letConst
                ( Plus'
                    (Mult' (Pow' (Var "c") k) base)
                    (Mult' (FloorDiv' (Minus' (Pow' (Var "c") k) (LitInt' 1)) (Minus' (Var "c") (LitInt' 1))) (LitInt' 2))
                )
            )
    run' prog `shouldBe` Right expected
