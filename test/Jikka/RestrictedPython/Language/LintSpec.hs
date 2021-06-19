{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Language.LintSpec (spec) where

import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Lint
import Jikka.RestrictedPython.Language.Util
import Test.Hspec

spec :: Spec
spec = do
  describe "hasSubscriptionInLoopCounters" $ do
    it "works on for-statements" $ do
      let prog =
            [ ToplevelFunctionDef
                "main"
                []
                IntTy
                [ For
                    (SubscriptTrg (NameTrg "a") (constIntExp 0))
                    (Call (Name "range") [constIntExp 100])
                    []
                ]
            ]
      let expected = True
      hasSubscriptionInLoopCounters prog `shouldBe` expected
    it "works on for-exprs" $ do
      let prog =
            [ ToplevelFunctionDef
                "main"
                []
                IntTy
                [ Return (ListComp (constIntExp 0) (Comprehension (SubscriptTrg (NameTrg "a") (constIntExp 0)) (Call (Name "range") [constIntExp 100]) Nothing))
                ]
            ]
      let expected = True
      hasSubscriptionInLoopCounters prog `shouldBe` expected
  describe "hasAssignmentToLoopCounters" $ do
    it "works" $ do
      let prog =
            [ ToplevelFunctionDef
                "main"
                []
                IntTy
                [ For
                    (NameTrg "i")
                    (Call (Name "range") [constIntExp 100])
                    [ AugAssign (NameTrg "i") Add (constIntExp 1)
                    ]
                ]
            ]
      let expected = True
      hasAssignmentToLoopCounters prog `shouldBe` expected
  describe "hasAssignmentToLoopIterators" $ do
    it "works" $ do
      let prog =
            [ ToplevelFunctionDef
                "main"
                []
                IntTy
                [ AnnAssign (NameTrg "a") (ListTy IntTy) (Call (Name "range") [constIntExp 100]),
                  For
                    (NameTrg "i")
                    (Name "a")
                    [ AnnAssign (SubscriptTrg (NameTrg "a") (constIntExp 5)) IntTy (Name "i")
                    ]
                ]
            ]
      let expected = True
      hasAssignmentToLoopIterators prog `shouldBe` expected
    it "works even if side effects are not trivial" $ do
      let prog =
            [ ToplevelFunctionDef
                "main"
                []
                IntTy
                [ AnnAssign (NameTrg "a") IntTy (constIntExp 0),
                  For
                    (NameTrg "i")
                    (Call (Name "f") [Name "a"])
                    [ AugAssign (NameTrg "a") Add (Name "i")
                    ]
                ]
            ]
      let expected = True
      hasAssignmentToLoopIterators prog `shouldBe` expected
  describe "hasReturnInLoops" $ do
    it "works" $ do
      let prog =
            [ ToplevelFunctionDef
                "main"
                []
                IntTy
                [ AnnAssign (NameTrg "a") (ListTy IntTy) (Call (Name "range") [constIntExp 10]),
                  For
                    (NameTrg "i")
                    (Name "a")
                    [ Return (constIntExp 0)
                    ]
                ]
            ]
      let expected = True
      hasReturnInLoops prog `shouldBe` expected
  describe "hasMixedAssignment" $ do
    it "works" $ do
      let prog =
            [ ToplevelFunctionDef
                "main"
                []
                IntTy
                [ AnnAssign (TupleTrg [NameTrg "a", SubscriptTrg (NameTrg "b") (constIntExp 0)]) (ListTy IntTy) (Call (Name "range") [constIntExp 10])
                ]
            ]
      let expected = True
      hasMixedAssignment prog `shouldBe` expected
