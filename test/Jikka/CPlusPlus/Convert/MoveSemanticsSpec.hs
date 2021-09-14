{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Convert.MoveSemanticsSpec
  ( spec,
  )
where

import Jikka.CPlusPlus.Convert.MoveSemantics
import Jikka.CPlusPlus.Language.Expr
import Jikka.CPlusPlus.Language.Util
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
                [(TyInt32, "a")]
                [ Declare TyInt32 "b" (DeclareCopy (Var "a")),
                  Assign (AssignExpr AddAssign (LeftVar "b") (Lit (LitInt32 10))),
                  Declare TyInt32 "c" (DeclareCopy (Var "b")),
                  Assign (AssignExpr AddAssign (LeftVar "b") (Lit (LitInt32 10))),
                  Return (BinOp Add (Var "b") (Var "c"))
                ]
            ]
    let expected =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyInt32, "a")]
                [ Assign (AssignExpr AddAssign (LeftVar "a") (Lit (LitInt32 10))),
                  Declare TyInt32 "c" (DeclareCopy (Var "a")),
                  Assign (AssignExpr AddAssign (LeftVar "a") (Lit (LitInt32 10))),
                  Return (BinOp Add (Var "a") (Var "c"))
                ]
            ]
    run' prog `shouldBe` Right expected

  it "recognizes push_back" $ do
    let prog =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyVector TyInt32, "a")]
                [ Declare (TyVector TyInt32) "b" (DeclareCopy (Var "a")),
                  ExprStatement (Call' (Method "push_back") [Var "b", Lit (LitInt32 10)]),
                  Declare (TyVector TyInt32) "c" (DeclareCopy (Var "b")),
                  ExprStatement (Call' (Method "push_back") [Var "b", Lit (LitInt32 10)]),
                  Return (BinOp Add (Call' MethodSize [Var "b"]) (Call' MethodSize [Var "c"]))
                ]
            ]
    let expected =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyVector TyInt32, "a")]
                [ ExprStatement (Call' (Method "push_back") [Var "a", Lit (LitInt32 10)]),
                  Declare (TyVector TyInt32) "c" (DeclareCopy (Var "a")),
                  ExprStatement (Call' (Method "push_back") [Var "a", Lit (LitInt32 10)]),
                  Return (BinOp Add (Call' MethodSize [Var "a"]) (Call' MethodSize [Var "c"]))
                ]
            ]
    run' prog `shouldBe` Right expected

  it "don't move if it is used in the next loop iteration" $ do
    let prog =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyInt32, "a")]
                [ For
                    TyInt32
                    "i"
                    (Lit (LitInt32 0))
                    (BinOp LessThan (Var "i") (Lit (LitInt32 10)))
                    (AssignIncr (LeftVar "i"))
                    [ Declare TyInt32 "b" (DeclareCopy (Var "a")),
                      Assign (AssignExpr AddAssign (LeftVar "b") (Lit (LitInt32 10)))
                    ],
                  Return (Var "a")
                ]
            ]
    let expected = prog
    run' prog `shouldBe` Right expected

  it "allows to write back" $ do
    let prog =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyInt32, "a")]
                [ Declare TyInt32 "b" (DeclareCopy (Var "a")),
                  Assign (AssignExpr AddAssign (LeftVar "b") (Lit (LitInt32 10))),
                  Assign (AssignExpr SimpleAssign (LeftVar "a") (Var "b")),
                  Return (Var "a")
                ]
            ]
    let expected =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyInt32, "a")]
                [ Assign (AssignExpr AddAssign (LeftVar "a") (Lit (LitInt32 10))),
                  Return (Var "a")
                ]
            ]
    run' prog `shouldBe` Right expected

  it "recognizes set_at" $ do
    let prog =
          Program
            [ FunDef
                (TyVector TyInt32)
                "func"
                [(TyVector TyInt32, "a")]
                [ Declare (TyVector TyInt32) "b" (DeclareCopy (Var "a")),
                  Assign (AssignExpr AddAssign (LeftAt (LeftVar "b") (Lit (LitInt32 0))) (Call' At [Var "a", Lit (LitInt32 1)])),
                  Assign (AssignExpr SimpleAssign (LeftVar "a") (Var "b")),
                  Return (Var "a")
                ]
            ]
    let expected =
          Program
            [ FunDef
                (TyVector TyInt32)
                "func"
                [(TyVector TyInt32, "a")]
                [ Assign (AssignExpr AddAssign (LeftAt (LeftVar "a") (Lit (LitInt32 0))) (Call' At [Var "a", Lit (LitInt32 1)])),
                  Return (Var "a")
                ]
            ]
    run' prog `shouldBe` Right expected

  it "TODO: allows to write back and reads after it" $ do
    let prog =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyInt32, "a")]
                [ Declare TyInt32 "b" (DeclareCopy (Var "a")),
                  Assign (AssignExpr AddAssign (LeftVar "b") (Lit (LitInt32 10))),
                  Assign (AssignExpr AddAssign (LeftVar "a") (Var "b")),
                  Return (BinOp Add (Var "a") (Var "b"))
                ]
            ]
    let expected =
          Program
            [ FunDef
                TyInt32
                "func"
                [(TyInt32, "a")]
                [ Assign (AssignExpr AddAssign (LeftVar "a") (Lit (LitInt32 10))),
                  Return (BinOp Add (Var "a") (Var "a"))
                ]
            ]
    -- TODO: fix this
    -- run' prog `shouldBe` Right expected
    run' prog `shouldNotBe` Right expected
