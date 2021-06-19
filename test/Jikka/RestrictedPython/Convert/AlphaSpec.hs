{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.AlphaSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Convert.Alpha (run)
import Jikka.RestrictedPython.Language.Expr
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let parsed =
          [ ToplevelFunctionDef
              "solve"
              [("x", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "y") IntTy (Name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "solve"
              [("x", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "y$0") IntTy (Name "x")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "distinguishes local variables in two diffrent functions" $ do
    let parsed =
          [ ToplevelFunctionDef
              "foo"
              [("x", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "y") IntTy (Name "x")
              ],
            ToplevelFunctionDef
              "bar"
              [("x", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "y") IntTy (Name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "foo"
              [("x", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "y$0") IntTy (Name "x")
              ],
            ToplevelFunctionDef
              "bar"
              [("x", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "y$1") IntTy (Name "x")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "distinguishes variables in two diffrent for-loops" $ do
    let parsed =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ For
                  (NameTrg "i")
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x") IntTy (Name "i")
                  ],
                For
                  (NameTrg "i")
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x") IntTy (Name "i")
                  ]
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ For
                  (NameTrg "i$0")
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x$1") IntTy (Name "i$0")
                  ],
                For
                  (NameTrg "i$2")
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x$3") IntTy (Name "i$2")
                  ]
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "removes underscoes" $ do
    let parsed =
          [ ToplevelAnnAssign
              "a"
              (ListTy IntTy)
              ( ListComp
                  (Constant (ConstInt 0))
                  ( Comprehension
                      (NameTrg "_")
                      (Call (Name "range") [Constant (ConstInt 10)])
                      Nothing
                  )
              )
          ]
    let expected =
          [ ToplevelAnnAssign
              "a"
              (ListTy IntTy)
              ( ListComp
                  (Constant (ConstInt 0))
                  ( Comprehension
                      (NameTrg "$0")
                      (Call (Name "range") [Constant (ConstInt 10)])
                      Nothing
                  )
              )
          ]
    run' parsed `shouldBe` Right expected
  it "works on recursive functions" $ do
    let parsed =
          [ ToplevelFunctionDef
              "f"
              [("x", IntTy)]
              IntTy
              [ Return (Call (Name "f") [Name "x"])
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "f"
              [("x", IntTy)]
              IntTy
              [ Return (Call (Name "f") [Name "x"])
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "doesn't rename type variables" $ do
    let parsed =
          [ ToplevelFunctionDef
              "f"
              [("x", VarTy "x")]
              (VarTy "f")
              [ Return (Call (Name "f") [Name "x"])
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "f"
              [("x", VarTy "x")]
              (VarTy "f")
              [ Return (Call (Name "f") [Name "x"])
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "makes repeated assignments for the same variable to single-assignments" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x") IntTy (Name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x$0") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$1") IntTy (Name "x$0"),
                AnnAssign (NameTrg "x$2") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$3") IntTy (Name "x$2"),
                AnnAssign (NameTrg "x$4") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$5") IntTy (Name "x$4")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "doesn't rename for augumented assignments" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AugAssign (NameTrg "x") Add (Constant (ConstInt 0)),
                AugAssign (NameTrg "x") Add (Name "x"),
                AugAssign (NameTrg "x") Add (Constant (ConstInt 0)),
                AugAssign (NameTrg "x") Add (Name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x$0") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$1") IntTy (Name "x$0"),
                AugAssign (NameTrg "x$1") Add (Constant (ConstInt 0)),
                AugAssign (NameTrg "x$1") Add (Name "x$1"),
                AugAssign (NameTrg "x$1") Add (Constant (ConstInt 0)),
                AugAssign (NameTrg "x$1") Add (Name "x$1")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "blames leaks of loop counters of for-statements" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ For
                  (NameTrg "i")
                  (List IntTy [])
                  [ Return (Name "i")
                  ],
                Return (Name "i")
              ]
          ]
    let expected = WithWrapped "Jikka.RestrictedPython.Convert.Alpha" (WithGroup SymbolError (Error "undefined identifier: i"))
    run' parsed `shouldBe` Left expected
  it "blames leaks of loop counters of for-statements even if variables with the same names are defined" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "i") IntTy (Constant (ConstInt 0)),
                For
                  (NameTrg "i")
                  (List IntTy [])
                  [ Return (Name "i")
                  ],
                Return (Name "i")
              ]
          ]
    let expected = WithWrapped "Jikka.RestrictedPython.Convert.Alpha" (WithGroup SemanticError (Error "cannot redefine variable: i"))
    run' parsed `shouldBe` Left expected
  it "doesn't leak loop counters of for-exprs" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "i") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "a") (ListTy IntTy) (ListComp (Constant (ConstInt 0)) (Comprehension (NameTrg "i") (List IntTy []) Nothing)),
                Return (Name "i")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "i$0") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "a$2") (ListTy IntTy) (ListComp (Constant (ConstInt 0)) (Comprehension (NameTrg "i$1") (List IntTy []) Nothing)),
                Return (Name "i$0")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "works with if-statements" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                If
                  (Name "x")
                  [ AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x") IntTy (Name "x"),
                    AnnAssign (NameTrg "y") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "y") IntTy (Name "y"),
                    AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x") IntTy (Name "x")
                  ]
                  [],
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x") IntTy (Name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x$0") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$1") IntTy (Name "x$0"),
                AnnAssign (NameTrg "x$2") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$3") IntTy (Name "x$2"),
                If
                  (Name "x$3")
                  [ AnnAssign (NameTrg "x$3") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x$3") IntTy (Name "x$3"),
                    AnnAssign (NameTrg "y$4") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "y$5") IntTy (Name "y$4"),
                    AnnAssign (NameTrg "x$3") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x$3") IntTy (Name "x$3")
                  ]
                  [],
                AnnAssign (NameTrg "x$6") IntTy (Name "x$3"),
                AnnAssign (NameTrg "x$7") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$8") IntTy (Name "x$7")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "works with for-loops" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                For
                  (NameTrg "i")
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x") IntTy (Name "x"),
                    AnnAssign (NameTrg "y") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "y") IntTy (Name "y"),
                    AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x") IntTy (Name "x")
                  ],
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x") IntTy (Name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x$0") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$1") IntTy (Name "x$0"),
                AnnAssign (NameTrg "x$2") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$3") IntTy (Name "x$2"),
                For
                  (NameTrg "i$4")
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x$3") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x$3") IntTy (Name "x$3"),
                    AnnAssign (NameTrg "y$5") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "y$6") IntTy (Name "y$5"),
                    AnnAssign (NameTrg "x$3") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x$3") IntTy (Name "x$3")
                  ],
                AnnAssign (NameTrg "x$7") IntTy (Name "x$3"),
                AnnAssign (NameTrg "x$8") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$9") IntTy (Name "x$8")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "doesn't rename subscripted assignments" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "a") (ListTy IntTy) (List IntTy []),
                AnnAssign (SubscriptTrg (NameTrg "a") (Constant (ConstInt 0))) IntTy (Subscript (Name "a") (Constant (ConstInt 0))),
                AnnAssign (SubscriptTrg (NameTrg "a") (Constant (ConstInt 0))) IntTy (Subscript (Name "a") (Constant (ConstInt 0))),
                AnnAssign (SubscriptTrg (NameTrg "a") (Constant (ConstInt 0))) IntTy (Subscript (Name "a") (Constant (ConstInt 0))),
                AnnAssign (SubscriptTrg (NameTrg "a") (Constant (ConstInt 0))) IntTy (Subscript (Name "a") (Constant (ConstInt 0)))
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "a$0") (ListTy IntTy) (List IntTy []),
                AnnAssign (SubscriptTrg (NameTrg "a$0") (Constant (ConstInt 0))) IntTy (Subscript (Name "a$0") (Constant (ConstInt 0))),
                AnnAssign (SubscriptTrg (NameTrg "a$0") (Constant (ConstInt 0))) IntTy (Subscript (Name "a$0") (Constant (ConstInt 0))),
                AnnAssign (SubscriptTrg (NameTrg "a$0") (Constant (ConstInt 0))) IntTy (Subscript (Name "a$0") (Constant (ConstInt 0))),
                AnnAssign (SubscriptTrg (NameTrg "a$0") (Constant (ConstInt 0))) IntTy (Subscript (Name "a$0") (Constant (ConstInt 0)))
              ]
          ]
    run' parsed `shouldBe` Right expected
