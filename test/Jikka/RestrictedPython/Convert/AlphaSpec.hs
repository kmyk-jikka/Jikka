{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.AlphaSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Convert.Alpha (run)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let parsed =
          [ ToplevelFunctionDef
              "f"
              [("n", IntTy)]
              IntTy
              [ If
                  (Compare (Name "n") (CmpOp' Eq' (VarTy "t")) (constIntExp 0))
                  [ Return (constIntExp 1)
                  ]
                  [ Return (BinOp (Name "n") Mult (Call (Name "f") [BinOp (Name "n") Sub (constIntExp 1)]))
                  ]
              ],
            ToplevelFunctionDef
              "solve"
              [("n", IntTy)]
              IntTy
              [ Return (BinOp (Call (Name "f") [Name "n"]) FloorMod (constIntExp 1000000007))
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "f"
              [("n$0", IntTy)]
              IntTy
              [ If
                  (Compare (Name "n$0") (CmpOp' Eq' (VarTy "t")) (constIntExp 0))
                  [ Return (constIntExp 1)
                  ]
                  [ Return (BinOp (Name "n$0") Mult (Call (Name "f") [BinOp (Name "n$0") Sub (constIntExp 1)]))
                  ]
              ],
            ToplevelFunctionDef
              "solve"
              [("n$1", IntTy)]
              IntTy
              [ Return (BinOp (Call (Name "f") [Name "n$1"]) FloorMod (constIntExp 1000000007))
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "fails with undefined variables" $ do
    let parsed =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ Return (Name "y")
              ]
          ]
    let expected = WithWrapped "Jikka.RestrictedPython.Convert.Alpha" (WithGroup SymbolError (Error "undefined identifier: y"))
    run' parsed `shouldBe` Left expected
  it "doesn't rename builtin functions " $ do
    let parsed =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ Return (Name "range")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ Return (Name "range")
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
              [("x$0", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "y$1") IntTy (Name "x$0")
              ],
            ToplevelFunctionDef
              "bar"
              [("x$2", IntTy)]
              IntTy
              [ AnnAssign (NameTrg "y$3") IntTy (Name "x$2")
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
                  (constIntExp 0)
                  ( Comprehension
                      (NameTrg "_")
                      (Call (Name "range") [constIntExp 10])
                      Nothing
                  )
              )
          ]
    let expected =
          [ ToplevelAnnAssign
              "a"
              (ListTy IntTy)
              ( ListComp
                  (constIntExp 0)
                  ( Comprehension
                      (NameTrg "$0")
                      (Call (Name "range") [constIntExp 10])
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
              [("x$0", IntTy)]
              IntTy
              [ Return (Call (Name "f") [Name "x$0"])
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
              [("x$0", VarTy "x")]
              (VarTy "f")
              [ Return (Call (Name "f") [Name "x$0"])
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "makes repeated assignments for the same variable to single-assignments for different variables" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x") IntTy (Name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x$0") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x$1") IntTy (Name "x$0"),
                AnnAssign (NameTrg "x$2") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x$3") IntTy (Name "x$2"),
                AnnAssign (NameTrg "x$4") IntTy (constIntExp 0),
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
              [ AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AugAssign (NameTrg "x") Add (constIntExp 0),
                AugAssign (NameTrg "x") Add (Name "x"),
                AugAssign (NameTrg "x") Add (constIntExp 0),
                AugAssign (NameTrg "x") Add (Name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x$0") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x$1") IntTy (Name "x$0"),
                AugAssign (NameTrg "x$1") Add (constIntExp 0),
                AugAssign (NameTrg "x$1") Add (Name "x$1"),
                AugAssign (NameTrg "x$1") Add (constIntExp 0),
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
              [ AnnAssign (NameTrg "i") IntTy (constIntExp 0),
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
  it "blames undefined variables which will be defined in the rest of the same loop" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ For
                  (NameTrg "i")
                  (List IntTy [])
                  [ Return (Name "a"),
                    AnnAssign (NameTrg "a") IntTy (constIntExp 0)
                  ]
              ]
          ]
    let expected = WithWrapped "Jikka.RestrictedPython.Convert.Alpha" (WithGroup SymbolError (Error "undefined identifier: a"))
    run' parsed `shouldBe` Left expected
  it "blames using variables which are defined in only one branch of if-statement" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ If
                  (constBoolExp True)
                  [ AnnAssign (NameTrg "a") IntTy (constIntExp 0)
                  ]
                  [],
                Return (Name "a")
              ]
          ]
    let expected = WithWrapped "Jikka.RestrictedPython.Convert.Alpha" (WithGroup SymbolError (Error "undefined identifier: a"))
    run' parsed `shouldBe` Left expected
  it "works with variables which are defined in both branches of if-statement" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ If
                  (constBoolExp True)
                  [ AnnAssign (NameTrg "a") IntTy (constIntExp 0)
                  ]
                  [ AnnAssign (NameTrg "a") IntTy (constIntExp 1)
                  ],
                Return (Name "a")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ If
                  (constBoolExp True)
                  [ AnnAssign (NameTrg "a$0") IntTy (constIntExp 0)
                  ]
                  [ AnnAssign (NameTrg "a$0") IntTy (constIntExp 1)
                  ],
                Return (Name "a$0")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "doesn't leak loop counters of for-exprs" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "i") IntTy (constIntExp 0),
                AnnAssign (NameTrg "a") (ListTy IntTy) (ListComp (constIntExp 0) (Comprehension (NameTrg "i") (List IntTy []) Nothing)),
                Return (Name "i")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "i$0") IntTy (constIntExp 0),
                AnnAssign (NameTrg "a$2") (ListTy IntTy) (ListComp (constIntExp 0) (Comprehension (NameTrg "i$1") (List IntTy []) Nothing)),
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
              [ AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                If
                  (Name "x")
                  [ AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "x") IntTy (Name "x"),
                    AnnAssign (NameTrg "y") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "y") IntTy (Name "y"),
                    AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "x") IntTy (Name "x")
                  ]
                  [],
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x") IntTy (Name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x$0") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x$1") IntTy (Name "x$0"),
                AnnAssign (NameTrg "x$2") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x$3") IntTy (Name "x$2"),
                If
                  (Name "x$3")
                  [ AnnAssign (NameTrg "x$3") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "x$3") IntTy (Name "x$3"),
                    AnnAssign (NameTrg "y$4") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "y$5") IntTy (Name "y$4"),
                    AnnAssign (NameTrg "x$3") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "x$3") IntTy (Name "x$3")
                  ]
                  [],
                AnnAssign (NameTrg "x$6") IntTy (Name "x$3"),
                AnnAssign (NameTrg "x$7") IntTy (constIntExp 0),
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
              [ AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                For
                  (NameTrg "i")
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "x") IntTy (Name "x"),
                    AnnAssign (NameTrg "y") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "y") IntTy (Name "y"),
                    AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "x") IntTy (Name "x")
                  ],
                AnnAssign (NameTrg "x") IntTy (Name "x"),
                AnnAssign (NameTrg "x") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x") IntTy (Name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "x$0") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x$1") IntTy (Name "x$0"),
                AnnAssign (NameTrg "x$2") IntTy (constIntExp 0),
                AnnAssign (NameTrg "x$3") IntTy (Name "x$2"),
                For
                  (NameTrg "i$4")
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x$3") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "x$3") IntTy (Name "x$3"),
                    AnnAssign (NameTrg "y$5") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "y$6") IntTy (Name "y$5"),
                    AnnAssign (NameTrg "x$3") IntTy (constIntExp 0),
                    AnnAssign (NameTrg "x$3") IntTy (Name "x$3")
                  ],
                AnnAssign (NameTrg "x$7") IntTy (Name "x$3"),
                AnnAssign (NameTrg "x$8") IntTy (constIntExp 0),
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
                AnnAssign (SubscriptTrg (NameTrg "a") (constIntExp 0)) IntTy (Subscript (Name "a") (constIntExp 0)),
                AnnAssign (SubscriptTrg (NameTrg "a") (constIntExp 0)) IntTy (Subscript (Name "a") (constIntExp 0)),
                AnnAssign (SubscriptTrg (NameTrg "a") (constIntExp 0)) IntTy (Subscript (Name "a") (constIntExp 0)),
                AnnAssign (SubscriptTrg (NameTrg "a") (constIntExp 0)) IntTy (Subscript (Name "a") (constIntExp 0))
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (NameTrg "a$0") (ListTy IntTy) (List IntTy []),
                AnnAssign (SubscriptTrg (NameTrg "a$0") (constIntExp 0)) IntTy (Subscript (Name "a$0") (constIntExp 0)),
                AnnAssign (SubscriptTrg (NameTrg "a$0") (constIntExp 0)) IntTy (Subscript (Name "a$0") (constIntExp 0)),
                AnnAssign (SubscriptTrg (NameTrg "a$0") (constIntExp 0)) IntTy (Subscript (Name "a$0") (constIntExp 0)),
                AnnAssign (SubscriptTrg (NameTrg "a$0") (constIntExp 0)) IntTy (Subscript (Name "a$0") (constIntExp 0))
              ]
          ]
    run' parsed `shouldBe` Right expected
