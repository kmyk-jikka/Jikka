{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.LinearFunctionSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.LinearFunction (run)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let xs = "xs"
    let ts2 = [IntTy, IntTy]
    let ts3 = [IntTy, IntTy, IntTy]
    let ts23 = [TupleTy ts3, TupleTy ts3]
    let proj i = Proj' ts3 i (Var xs)
    let prog =
          ResultExpr
            ( Let
                "c"
                IntTy
                (LitInt' 10)
                ( Lam
                    [(xs, TupleTy ts3)]
                    (Tuple' ts2 [Plus' (proj 0) (Mult' (Var "c") (proj 1)), Plus' (proj 0) (proj 2)])
                )
            )
    let expected =
          ResultExpr
            ( Let
                "c"
                IntTy
                (LitInt' 10)
                ( Lam
                    [(xs, TupleTy ts3)]
                    (MatAp' 2 3 (Tuple' ts23 [Tuple' ts3 [Lit1, Mult' (Var "c") Lit1, Lit0], Tuple' ts3 [Lit1, Lit0, Lit1]]) (Var xs))
                )
            )
    run' prog `shouldBe` Right expected
