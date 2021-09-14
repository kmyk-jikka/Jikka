{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Convert.UnpackTuplesSpec
  ( spec,
  )
where

import qualified Jikka.CPlusPlus.Convert.BurnFlavouredNames as Y_BurnFlavouredNames
import Jikka.CPlusPlus.Convert.UnpackTuples
import Jikka.CPlusPlus.Language.Expr
import Jikka.CPlusPlus.Language.Util
import Jikka.Common.Alpha
import Jikka.Common.Error
import Test.Hspec

run' :: Program -> Either Error Program
run' prog = flip evalAlphaT 0 $ do
  prog <- run prog
  Y_BurnFlavouredNames.run prog

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          Program
            [ FunDef
                TyInt
                "func"
                [(TyInt, "a")]
                [ Declare (TyTuple [TyInt, TyBool]) "b" (DeclareCopy (Call' (StdTuple [TyInt, TyBool]) [Var "a", Lit (LitBool True)])),
                  Return (BinOp Add (Call' (StdGet 0) [Var "b"]) (Call' (StdGet 1) [Var "b"]))
                ]
            ]
    let expected =
          Program
            [ FunDef
                TyInt
                "func"
                [(TyInt, "a")]
                [ Declare TyInt "b" (DeclareCopy (Var "a")),
                  Declare TyBool "b2" (DeclareCopy (Lit (LitBool True))),
                  Return (BinOp Add (Var "b") (Var "b2"))
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
                [ Declare (TyArray TyInt32 3) "b" (DeclareCopy (Call' (ArrayExt TyInt32) [Var "a", Lit (LitInt32 10), Lit (LitInt32 15)])),
                  Return (BinOp Add (Call' At [Var "b", Lit (LitInt32 0)]) (Call' At [Var "b", Lit (LitInt32 2)]))
                ]
            ]
    let expected =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyInt32, "a")]
                [ Declare TyInt32 "b" (DeclareCopy (Var "a")),
                  Declare TyInt32 "b2" (DeclareCopy (Lit (LitInt32 10))),
                  Declare TyInt32 "b3" (DeclareCopy (Lit (LitInt32 15))),
                  Return (BinOp Add (Var "b") (Var "b3"))
                ]
            ]
    run' prog `shouldBe` Right expected
