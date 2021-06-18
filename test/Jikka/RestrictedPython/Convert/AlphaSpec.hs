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
              "solve$0"
              [("x$1", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "y$2") IntTy (Name "x$1")
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
              "foo$0"
              [("x$1", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "y$2") IntTy (Name "x$1")
              ],
            ToplevelFunctionDef
              "bar$3"
              [("x$4", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "y$5") IntTy (Name "x$4")
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
              "solve$0"
              []
              IntTy
              [ For
                  (NameTrg "i$1")
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x$2") IntTy (Name "i$1")
                  ],
                For
                  (NameTrg "i$3")
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x$4") IntTy (Name "i$3")
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
              "a$0"
              (ListTy IntTy)
              ( ListComp
                  (Constant (ConstInt 0))
                  ( Comprehension
                      (NameTrg "$1")
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
              "f$0"
              [("x$1", IntTy)]
              IntTy
              [ Return (Call (Name "f$0") [Name "x$1"])
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
              "f$0"
              [("x$1", VarTy "x")]
              (VarTy "f")
              [ Return (Call (Name "f$0") [Name "x$1"])
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
              "main$0"
              []
              IntTy
              [ AnnAssign (NameTrg "x$1") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$2") IntTy (Name "x$1"),
                AnnAssign (NameTrg "x$3") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$4") IntTy (Name "x$3"),
                AnnAssign (NameTrg "x$5") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$6") IntTy (Name "x$5")
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
              "main$0"
              []
              IntTy
              [ AnnAssign (NameTrg "x$1") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$2") IntTy (Name "x$1"),
                AugAssign (NameTrg "x$2") Add (Constant (ConstInt 0)),
                AugAssign (NameTrg "x$2") Add (Name "x$2"),
                AugAssign (NameTrg "x$2") Add (Constant (ConstInt 0)),
                AugAssign (NameTrg "x$2") Add (Name "x$2")
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
              "main$0"
              []
              IntTy
              [ AnnAssign (NameTrg "i$1") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "a$3") (ListTy IntTy) (ListComp (Constant (ConstInt 0)) (Comprehension (NameTrg "i$2") (List IntTy []) Nothing)),
                Return (Name "i$1")
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
              "main$0"
              []
              IntTy
              [ AnnAssign (NameTrg "x$1") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$2") IntTy (Name "x$1"),
                AnnAssign (NameTrg "x$3") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$4") IntTy (Name "x$3"),
                If
                  (Name "x$4")
                  [ AnnAssign (NameTrg "x$4") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x$4") IntTy (Name "x$4"),
                    AnnAssign (NameTrg "y$5") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "y$6") IntTy (Name "y$5"),
                    AnnAssign (NameTrg "x$4") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x$4") IntTy (Name "x$4")
                  ]
                  [],
                AnnAssign (NameTrg "x$7") IntTy (Name "x$4"),
                AnnAssign (NameTrg "x$8") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$9") IntTy (Name "x$8")
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
              "main$0"
              []
              IntTy
              [ AnnAssign (NameTrg "x$1") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$2") IntTy (Name "x$1"),
                AnnAssign (NameTrg "x$3") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$4") IntTy (Name "x$3"),
                For
                  (NameTrg "i$5")
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x$4") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x$4") IntTy (Name "x$4"),
                    AnnAssign (NameTrg "y$6") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "y$7") IntTy (Name "y$6"),
                    AnnAssign (NameTrg "x$4") IntTy (Constant (ConstInt 0)),
                    AnnAssign (NameTrg "x$4") IntTy (Name "x$4")
                  ],
                AnnAssign (NameTrg "x$8") IntTy (Name "x$4"),
                AnnAssign (NameTrg "x$9") IntTy (Constant (ConstInt 0)),
                AnnAssign (NameTrg "x$10") IntTy (Name "x$9")
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
              "main$0"
              []
              IntTy
              [ AnnAssign (NameTrg "a$1") (ListTy IntTy) (List IntTy []),
                AnnAssign (SubscriptTrg (NameTrg "a$1") (Constant (ConstInt 0))) IntTy (Subscript (Name "a$1") (Constant (ConstInt 0))),
                AnnAssign (SubscriptTrg (NameTrg "a$1") (Constant (ConstInt 0))) IntTy (Subscript (Name "a$1") (Constant (ConstInt 0))),
                AnnAssign (SubscriptTrg (NameTrg "a$1") (Constant (ConstInt 0))) IntTy (Subscript (Name "a$1") (Constant (ConstInt 0))),
                AnnAssign (SubscriptTrg (NameTrg "a$1") (Constant (ConstInt 0))) IntTy (Subscript (Name "a$1") (Constant (ConstInt 0)))
              ]
          ]
    run' parsed `shouldBe` Right expected
