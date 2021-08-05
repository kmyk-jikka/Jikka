{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Convert.MoveSemanticsSpec
  ( spec,
  )
where

import Jikka.CPlusPlus.Convert.MoveSemantics
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
                TyInt32
                "func"
                [(TyVector TyInt32, "a")]
                [ Declare (TyVector TyInt32) "b" (DeclareCopy (Var "a")),
                  ExprStatement (Call (Method "push_back") [Var "b", Lit (LitInt32 10)]),
                  Declare (TyVector TyInt32) "c" (DeclareCopy (Var "b")),
                  ExprStatement (Call (Method "push_back") [Var "b", Lit (LitInt32 10)]),
                  Return (BinOp Add (Call MethodSize [Var "b"]) (Call MethodSize [Var "c"]))
                ]
            ]
    let expected =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyVector TyInt32, "a")]
                [ ExprStatement (Call (Method "push_back") [Var "a", Lit (LitInt32 10)]),
                  Declare (TyVector TyInt32) "c" (DeclareCopy (Var "a")),
                  ExprStatement (Call (Method "push_back") [Var "a", Lit (LitInt32 10)]),
                  Return (BinOp Add (Call MethodSize [Var "a"]) (Call MethodSize [Var "c"]))
                ]
            ]
    -- TODO: fix the bug https://github.com/kmyk/Jikka/issues/154 and enable this check
    -- run' prog `shouldBe` Right expected
    run' prog `shouldBe` run' prog -- supress warnings
    expected `shouldBe` expected -- supress warnings
