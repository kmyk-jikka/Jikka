{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.AlphaSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Convert.Alpha (run)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.WithoutLoc
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
                  (eqExp (VarTy "t") (name "n") (constIntExp 0))
                  [ Return (constIntExp 1)
                  ]
                  [ Return (multExp (name "n") (call (name "f") [subExp (name "n") (constIntExp 1)]))
                  ]
              ],
            ToplevelFunctionDef
              "solve"
              [("n", IntTy)]
              IntTy
              [ Return (binOp (call (name "f") [name "n"]) FloorMod (constIntExp 1000000007))
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "f"
              [("n$0", IntTy)]
              IntTy
              [ If
                  (eqExp (VarTy "t") (name "n$0") (constIntExp 0))
                  [ Return (constIntExp 1)
                  ]
                  [ Return (multExp (name "n$0") (call (name "f") [subExp (name "n$0") (constIntExp 1)]))
                  ]
              ],
            ToplevelFunctionDef
              "solve"
              [("n$1", IntTy)]
              IntTy
              [ Return (binOp (call (name "f") [name "n$1"]) FloorMod (constIntExp 1000000007))
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "fails with undefined variables" $ do
    let parsed =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ Return (name "y")
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
              [ Return (name "range")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ Return (name "range")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "distinguishes local variables in two diffrent functions" $ do
    let parsed =
          [ ToplevelFunctionDef
              "foo"
              [("x", IntTy)]
              IntTy
              [ AnnAssign (nameTrg "y") IntTy (name "x")
              ],
            ToplevelFunctionDef
              "bar"
              [("x", IntTy)]
              IntTy
              [ AnnAssign (nameTrg "y") IntTy (name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "foo"
              [("x$0", IntTy)]
              IntTy
              [ AnnAssign (nameTrg "y$1") IntTy (name "x$0")
              ],
            ToplevelFunctionDef
              "bar"
              [("x$2", IntTy)]
              IntTy
              [ AnnAssign (nameTrg "y$3") IntTy (name "x$2")
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
                  (nameTrg "i")
                  (list IntTy [])
                  [ AnnAssign (nameTrg "x") IntTy (name "i")
                  ],
                For
                  (nameTrg "i")
                  (list IntTy [])
                  [ AnnAssign (nameTrg "x") IntTy (name "i")
                  ]
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "solve"
              []
              IntTy
              [ For
                  (nameTrg "i$0")
                  (list IntTy [])
                  [ AnnAssign (nameTrg "x$1") IntTy (name "i$0")
                  ],
                For
                  (nameTrg "i$2")
                  (list IntTy [])
                  [ AnnAssign (nameTrg "x$3") IntTy (name "i$2")
                  ]
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "removes underscoes" $ do
    let parsed =
          [ ToplevelAnnAssign
              "a"
              (ListTy IntTy)
              ( listComp
                  (constIntExp 0)
                  ( Comprehension
                      (nameTrg "_")
                      (call (name "range") [constIntExp 10])
                      Nothing
                  )
              )
          ]
    let expected =
          [ ToplevelAnnAssign
              "a"
              (ListTy IntTy)
              ( listComp
                  (constIntExp 0)
                  ( Comprehension
                      (nameTrg "$0")
                      (call (name "range") [constIntExp 10])
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
              [ Return (call (name "f") [name "x"])
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "f"
              [("x$0", IntTy)]
              IntTy
              [ Return (call (name "f") [name "x$0"])
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "doesn't rename type variables" $ do
    let parsed =
          [ ToplevelFunctionDef
              "f"
              [("x", VarTy "x")]
              (VarTy "f")
              [ Return (call (name "f") [name "x"])
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "f"
              [("x$0", VarTy "x")]
              (VarTy "f")
              [ Return (call (name "f") [name "x$0"])
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "makes repeated assignments for the same variable to single-assignments for different variables" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x") IntTy (name "x"),
                AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x") IntTy (name "x"),
                AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x") IntTy (name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "x$0") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x$1") IntTy (name "x$0"),
                AnnAssign (nameTrg "x$2") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x$3") IntTy (name "x$2"),
                AnnAssign (nameTrg "x$4") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x$5") IntTy (name "x$4")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "doesn't rename for augumented assignments" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x") IntTy (name "x"),
                AugAssign (nameTrg "x") Add (constIntExp 0),
                AugAssign (nameTrg "x") Add (name "x"),
                AugAssign (nameTrg "x") Add (constIntExp 0),
                AugAssign (nameTrg "x") Add (name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "x$0") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x$1") IntTy (name "x$0"),
                AugAssign (nameTrg "x$1") Add (constIntExp 0),
                AugAssign (nameTrg "x$1") Add (name "x$1"),
                AugAssign (nameTrg "x$1") Add (constIntExp 0),
                AugAssign (nameTrg "x$1") Add (name "x$1")
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
                  (nameTrg "i")
                  (list IntTy [])
                  [ Return (name "i")
                  ],
                Return (name "i")
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
              [ AnnAssign (nameTrg "i") IntTy (constIntExp 0),
                For
                  (nameTrg "i")
                  (list IntTy [])
                  [ Return (name "i")
                  ],
                Return (name "i")
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
                  (nameTrg "i")
                  (list IntTy [])
                  [ Return (name "a"),
                    AnnAssign (nameTrg "a") IntTy (constIntExp 0)
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
                  [ AnnAssign (nameTrg "a") IntTy (constIntExp 0)
                  ]
                  [],
                Return (name "a")
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
                  [ AnnAssign (nameTrg "a") IntTy (constIntExp 0)
                  ]
                  [ AnnAssign (nameTrg "a") IntTy (constIntExp 1)
                  ],
                Return (name "a")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ If
                  (constBoolExp True)
                  [ AnnAssign (nameTrg "a$0") IntTy (constIntExp 0)
                  ]
                  [ AnnAssign (nameTrg "a$0") IntTy (constIntExp 1)
                  ],
                Return (name "a$0")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "doesn't leak loop counters of for-exprs" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "i") IntTy (constIntExp 0),
                AnnAssign (nameTrg "a") (ListTy IntTy) (listComp (constIntExp 0) (Comprehension (nameTrg "i") (list IntTy []) Nothing)),
                Return (name "i")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "i$0") IntTy (constIntExp 0),
                AnnAssign (nameTrg "a$2") (ListTy IntTy) (listComp (constIntExp 0) (Comprehension (nameTrg "i$1") (list IntTy []) Nothing)),
                Return (name "i$0")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "works with if-statements" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x") IntTy (name "x"),
                AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x") IntTy (name "x"),
                If
                  (name "x")
                  [ AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "x") IntTy (name "x"),
                    AnnAssign (nameTrg "y") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "y") IntTy (name "y"),
                    AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "x") IntTy (name "x")
                  ]
                  [],
                AnnAssign (nameTrg "x") IntTy (name "x"),
                AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x") IntTy (name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "x$0") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x$1") IntTy (name "x$0"),
                AnnAssign (nameTrg "x$2") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x$3") IntTy (name "x$2"),
                If
                  (name "x$3")
                  [ AnnAssign (nameTrg "x$3") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "x$3") IntTy (name "x$3"),
                    AnnAssign (nameTrg "y$4") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "y$5") IntTy (name "y$4"),
                    AnnAssign (nameTrg "x$3") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "x$3") IntTy (name "x$3")
                  ]
                  [],
                AnnAssign (nameTrg "x$6") IntTy (name "x$3"),
                AnnAssign (nameTrg "x$7") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x$8") IntTy (name "x$7")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "works with for-loops" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x") IntTy (name "x"),
                AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x") IntTy (name "x"),
                For
                  (nameTrg "i")
                  (list IntTy [])
                  [ AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "x") IntTy (name "x"),
                    AnnAssign (nameTrg "y") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "y") IntTy (name "y"),
                    AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "x") IntTy (name "x")
                  ],
                AnnAssign (nameTrg "x") IntTy (name "x"),
                AnnAssign (nameTrg "x") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x") IntTy (name "x")
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "x$0") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x$1") IntTy (name "x$0"),
                AnnAssign (nameTrg "x$2") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x$3") IntTy (name "x$2"),
                For
                  (nameTrg "i$4")
                  (list IntTy [])
                  [ AnnAssign (nameTrg "x$3") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "x$3") IntTy (name "x$3"),
                    AnnAssign (nameTrg "y$5") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "y$6") IntTy (name "y$5"),
                    AnnAssign (nameTrg "x$3") IntTy (constIntExp 0),
                    AnnAssign (nameTrg "x$3") IntTy (name "x$3")
                  ],
                AnnAssign (nameTrg "x$7") IntTy (name "x$3"),
                AnnAssign (nameTrg "x$8") IntTy (constIntExp 0),
                AnnAssign (nameTrg "x$9") IntTy (name "x$8")
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "doesn't rename subscripted assignments" $ do
    let parsed =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "a") (ListTy IntTy) (list IntTy []),
                AnnAssign (subscriptTrg (nameTrg "a") (constIntExp 0)) IntTy (subscript (name "a") (constIntExp 0)),
                AnnAssign (subscriptTrg (nameTrg "a") (constIntExp 0)) IntTy (subscript (name "a") (constIntExp 0)),
                AnnAssign (subscriptTrg (nameTrg "a") (constIntExp 0)) IntTy (subscript (name "a") (constIntExp 0)),
                AnnAssign (subscriptTrg (nameTrg "a") (constIntExp 0)) IntTy (subscript (name "a") (constIntExp 0))
              ]
          ]
    let expected =
          [ ToplevelFunctionDef
              "main"
              []
              IntTy
              [ AnnAssign (nameTrg "a$0") (ListTy IntTy) (list IntTy []),
                AnnAssign (subscriptTrg (nameTrg "a$0") (constIntExp 0)) IntTy (subscript (name "a$0") (constIntExp 0)),
                AnnAssign (subscriptTrg (nameTrg "a$0") (constIntExp 0)) IntTy (subscript (name "a$0") (constIntExp 0)),
                AnnAssign (subscriptTrg (nameTrg "a$0") (constIntExp 0)) IntTy (subscript (name "a$0") (constIntExp 0)),
                AnnAssign (subscriptTrg (nameTrg "a$0") (constIntExp 0)) IntTy (subscript (name "a$0") (constIntExp 0))
              ]
          ]
    run' parsed `shouldBe` Right expected
