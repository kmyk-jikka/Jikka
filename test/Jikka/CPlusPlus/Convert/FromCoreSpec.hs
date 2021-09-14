{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Convert.FromCoreSpec
  ( spec,
  )
where

import qualified Jikka.CPlusPlus.Convert.BurnFlavouredNames as Y_BurnFlavouredNames
import Jikka.CPlusPlus.Convert.FromCore
import qualified Jikka.CPlusPlus.Language.Expr as Y
import qualified Jikka.CPlusPlus.Language.Util as Y
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Language.BuiltinPatterns as X
import qualified Jikka.Core.Language.Expr as X
import Test.Hspec

run' :: X.Program -> Either Error Y.Program
run' prog = flip evalAlphaT 0 $ do
  prog <- run prog
  Y_BurnFlavouredNames.run prog

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
            "f"
            [(Y.TyInt64, "n")]
            [ Y.Declare Y.TyInt64 "x" Y.DeclareDefault,
              Y.If
                (Y.BinOp Y.Equal (Y.Var "n") (Y.Lit (Y.LitInt64 0)))
                [Y.Assign (Y.AssignExpr Y.SimpleAssign (Y.LeftVar "x") (Y.Lit (Y.LitInt64 1)))]
                ( Just
                    [ Y.Assign
                        ( Y.AssignExpr
                            Y.SimpleAssign
                            (Y.LeftVar "x")
                            ( Y.BinOp
                                Y.Mul
                                (Y.Var "n")
                                ( Y.Call'
                                    (Y.Function "f" [])
                                    [Y.BinOp Y.Sub (Y.Var "n") (Y.Lit (Y.LitInt64 1))]
                                )
                            )
                        )
                    ]
                ),
              Y.Return (Y.Var "x")
            ]
    let expectedSolve =
          Y.FunDef
            Y.TyInt64
            "solve"
            [(Y.TyInt64, "a")]
            [Y.Return (Y.Call' (Y.Function "f" []) [Y.Var "a"])]
    let expected = Y.Program [expectedF, expectedSolve]
    run' prog `shouldBe` Right expected
