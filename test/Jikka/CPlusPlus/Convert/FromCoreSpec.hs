{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Convert.FromCoreSpec
  ( spec,
  )
where

import Jikka.CPlusPlus.Convert.FromCore
import qualified Jikka.CPlusPlus.Language.Expr as Y
import qualified Jikka.Core.Language.BuiltinPatterns as X
import qualified Jikka.Core.Language.Expr as X
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          X.ToplevelLet
            X.Rec
            "f"
            [("n", X.IntTy)]
            X.IntTy
            ( X.If'
                X.IntTy
                (X.Equal' X.IntTy (X.Var "n") X.Lit0)
                X.Lit1
                (X.Mult' (X.Var "n") (X.App (X.Var "f") [X.Minus' (X.Var "n") X.Lit1]))
            )
            (X.ResultExpr (X.Var "f"))
    let expectedF =
          Y.FunDef
            Y.TyInt64
            "f0_f"
            [(Y.TyInt64, "a1_n")]
            [ Y.If
                (Y.BinOp Y.Equal (Y.Var "a1_n") (Y.Lit (Y.LitInt64 0)))
                [Y.Return (Y.Lit (Y.LitInt64 1))]
                ( Just
                    [ Y.Return
                        ( Y.BinOp
                            Y.Mul
                            (Y.Var "a1_n")
                            ( Y.Call
                                (Y.Callable (Y.Var "f0_f"))
                                [Y.BinOp Y.Sub (Y.Var "a1_n") (Y.Lit (Y.LitInt64 1))]
                            )
                        )
                    ]
                )
            ]
    let expectedSolve =
          Y.FunDef
            Y.TyInt64
            "solve"
            [(Y.TyInt64, "a2")]
            [Y.Return (Y.Call (Y.Callable (Y.Var "f0_f")) [Y.Var "a2"])]
    let expected = Y.Program [expectedF, expectedSolve]
    run prog `shouldBe` Right expected
