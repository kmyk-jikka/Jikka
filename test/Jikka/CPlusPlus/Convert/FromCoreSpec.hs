{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Convert.FromCoreSpec
  ( spec,
  )
where

import Jikka.CPlusPlus.Convert.FromCore
import qualified Jikka.CPlusPlus.Language.Expr as Y
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Language.BuiltinPatterns as X
import qualified Jikka.Core.Language.Expr as X
import Test.Hspec

run' :: X.Program -> Either Error Y.Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          X.ToplevelLetRec
            "f"
            [("n", X.IntTy)]
            X.IntTy
            ( X.If'
                X.IntTy
                (X.Equal' X.IntTy (X.Var "n") X.Lit0)
                X.Lit1
                (X.Mult' (X.Var "n") (X.App (X.Var "f") (X.Minus' (X.Var "n") X.Lit1)))
            )
            (X.ResultExpr (X.Var "f"))
    let expectedF =
          Y.FunDef
            Y.TyInt64
            "f_0"
            [(Y.TyInt64, "n_1")]
            [ Y.Declare Y.TyInt64 "x2" Y.DeclareDefault,
              Y.If
                (Y.BinOp Y.Equal (Y.Var "n_1") (Y.Lit (Y.LitInt64 0)))
                [Y.Assign (Y.AssignExpr Y.SimpleAssign (Y.LeftVar "x2") (Y.Lit (Y.LitInt64 1)))]
                ( Just
                    [ Y.Assign
                        ( Y.AssignExpr
                            Y.SimpleAssign
                            (Y.LeftVar "x2")
                            ( Y.BinOp
                                Y.Mul
                                (Y.Var "n_1")
                                ( Y.CallExpr
                                    (Y.Var "f_0")
                                    [Y.BinOp Y.Sub (Y.Var "n_1") (Y.Lit (Y.LitInt64 1))]
                                )
                            )
                        )
                    ]
                ),
              Y.Return (Y.Var "x2")
            ]
    let expectedSolve =
          Y.FunDef
            Y.TyInt64
            "solve"
            [(Y.TyInt64, "a3")]
            [Y.Return (Y.CallExpr (Y.Var "f_0") [Y.Var "a3"])]
    let expected = Y.Program [expectedF, expectedSolve]
    run' prog `shouldBe` Right expected
