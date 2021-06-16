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
run' = evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let parsed =
          [ ToplevelFunctionDef
              "solve"
              [("x", IntTy)]
              [ AnnAssign (NameTrg "y") IntTy (Name "x")
              ]
              IntTy
          ]
    let expected =
          [ ToplevelFunctionDef
              "solve$0"
              [("x$1", IntTy)]
              [ AnnAssign (NameTrg "y$2") IntTy (Name "x$1")
              ]
              IntTy
          ]
    run' parsed `shouldBe` Right expected
  it "distinguishes local variables in two diffrent functions" $ do
    let parsed =
          [ ToplevelFunctionDef
              "foo"
              [("x", IntTy)]
              [ AnnAssign (NameTrg "y") IntTy (Name "x")
              ]
              IntTy,
            ToplevelFunctionDef
              "bar"
              [("x", IntTy)]
              [ AnnAssign (NameTrg "y") IntTy (Name "x")
              ]
              IntTy
          ]
    let expected =
          [ ToplevelFunctionDef
              "foo$0"
              [("x$1", IntTy)]
              [ AnnAssign (NameTrg "y$2") IntTy (Name "x$1")
              ]
              IntTy,
            ToplevelFunctionDef
              "bar$3"
              [("x$4", IntTy)]
              [ AnnAssign (NameTrg "y$5") IntTy (Name "x$4")
              ]
              IntTy
          ]
    run' parsed `shouldBe` Right expected
  it "distinguishes variables in two diffrent for-loops" $ do
    let parsed =
          [ ToplevelFunctionDef
              "solve"
              []
              [ For
                  (NameTrg "i")
                  IntTy
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x") IntTy (Name "i")
                  ],
                For
                  (NameTrg "i")
                  IntTy
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x") IntTy (Name "i")
                  ]
              ]
              IntTy
          ]
    let expected =
          [ ToplevelFunctionDef
              "solve$0"
              []
              [ For
                  (NameTrg "i$1")
                  IntTy
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x$2") IntTy (Name "i$1")
                  ],
                For
                  (NameTrg "i$3")
                  IntTy
                  (List IntTy [])
                  [ AnnAssign (NameTrg "x$4") IntTy (Name "i$3")
                  ]
              ]
              IntTy
          ]
    run' parsed `shouldBe` Right expected
  it "removes underscoes" $ do
    let parsed =
          [ ToplevelAnnAssign
              "a"
              (ListTy IntTy)
              ( ListComp
                  IntTy
                  (Constant (ConstInt 0))
                  ( Comprehension
                      (NameTrg "_")
                      IntTy
                      (Call (ListTy IntTy) (Name "range") [Constant (ConstInt 10)])
                      Nothing
                  )
              )
          ]
    let expected =
          [ ToplevelAnnAssign
              "a$0"
              (ListTy IntTy)
              ( ListComp
                  IntTy
                  (Constant (ConstInt 0))
                  ( Comprehension
                      (NameTrg "$1")
                      IntTy
                      (Call (ListTy IntTy) (Name "range") [Constant (ConstInt 10)])
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
              [ Return (Call IntTy (Name "f") [Name "x"])
              ]
              IntTy
          ]
    let expected =
          [ ToplevelFunctionDef
              "f$0"
              [("x$1", IntTy)]
              [ Return (Call IntTy (Name "f$0") [Name "x$1"])
              ]
              IntTy
          ]
    run' parsed `shouldBe` Right expected
  it "doesn't rename type variables" $ do
    let parsed =
          [ ToplevelFunctionDef
              "f"
              [("x", VarTy "x")]
              [ Return (Call (VarTy "t") (Name "f") [Name "x"])
              ]
              (VarTy "f")
          ]
    let expected =
          [ ToplevelFunctionDef
              "f$0"
              [("x$1", VarTy "x")]
              [ Return (Call (VarTy "t") (Name "f$0") [Name "x$1"])
              ]
              (VarTy "f")
          ]
    run' parsed `shouldBe` Right expected
