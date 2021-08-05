{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Convert.UnpackTuplesSpec
  ( spec,
  )
where

import Jikka.CPlusPlus.Convert.UnpackTuples
import Jikka.CPlusPlus.Language.Expr
import Jikka.Common.Alpha
import Jikka.Common.Error
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          Program
            [ FunDef
                TyInt
                "func"
                [(TyInt, "a")]
                [ Declare (TyTuple [TyInt, TyBool]) "b" (DeclareCopy (Call (StdTuple [TyInt, TyBool]) [Var "a", Lit (LitBool True)])),
                  Return (BinOp Add (Call (StdGet 0) [Var "b"]) (Call (StdGet 1) [Var "b"]))
                ]
            ]
    let expected =
          Program
            [ FunDef
                TyInt
                "func"
                [(TyInt, "a")]
                [ Declare TyInt "b_0" (DeclareCopy (Var "a")),
                  Declare TyBool "b_1" (DeclareCopy (Lit (LitBool True))),
                  Return (BinOp Add (Var "b_0") (Var "b_1"))
                ]
            ]
    run' prog `shouldBe` Right expected
  it "works on an array" $ do
    let prog =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyInt32, "a")]
                [ Declare (TyArray TyInt32 3) "b" (DeclareCopy (Call (ArrayExt TyInt32) [Var "a", Lit (LitInt32 10), Lit (LitInt32 15)])),
                  Return (BinOp Add (Call At [Var "b", Lit (LitInt32 0)]) (Call At [Var "b", Lit (LitInt32 2)]))
                ]
            ]
    let expected =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyInt32, "a")]
                [ Declare TyInt32 "b_0" (DeclareCopy (Var "a")),
                  Declare TyInt32 "b_1" (DeclareCopy (Lit (LitInt32 10))),
                  Declare TyInt32 "b_2" (DeclareCopy (Lit (LitInt32 15))),
                  Return (BinOp Add (Var "b_0") (Var "b_2"))
                ]
            ]
    run' prog `shouldBe` Right expected
