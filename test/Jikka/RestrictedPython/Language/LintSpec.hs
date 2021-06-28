{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Language.LintSpec (spec) where

import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Lint
import Jikka.RestrictedPython.Language.Util (toplevelMainDef)
import Jikka.RestrictedPython.Language.WithoutLoc
import Test.Hspec

spec :: Spec
spec = do
  describe "hasSubscriptionInLoopCounters" $ do
    it "works on for-statements" $ do
      let prog =
            toplevelMainDef
              [ For
                  (subscriptTrg (nameTrg "a") (constIntExp 0))
                  (call (name "range") [constIntExp 100])
                  []
              ]
      let expected = True
      hasSubscriptionInLoopCounters prog `shouldBe` expected
    it "works on for-exprs" $ do
      let prog =
            toplevelMainDef
              [ Return (listComp (constIntExp 0) (Comprehension (subscriptTrg (nameTrg "a") (constIntExp 0)) (call (name "range") [constIntExp 100]) Nothing))
              ]
      let expected = True
      hasSubscriptionInLoopCounters prog `shouldBe` expected
  describe "hasAssignmentToLoopCounters" $ do
    it "works" $ do
      let prog =
            toplevelMainDef
              [ For
                  (nameTrg "i")
                  (call (name "range") [constIntExp 100])
                  [ AugAssign (nameTrg "i") Add (constIntExp 1)
                  ]
              ]
      let expected = True
      hasAssignmentToLoopCounters prog `shouldBe` expected
  describe "hasAssignmentToLoopIterators" $ do
    it "works" $ do
      let prog =
            toplevelMainDef
              [ AnnAssign (nameTrg "a") (ListTy IntTy) (call (name "range") [constIntExp 100]),
                For
                  (nameTrg "i")
                  (name "a")
                  [ AnnAssign (subscriptTrg (nameTrg "a") (constIntExp 5)) IntTy (name "i")
                  ]
              ]
      let expected = True
      hasAssignmentToLoopIterators prog `shouldBe` expected
    it "works even if side effects are not trivial" $ do
      let prog =
            toplevelMainDef
              [ AnnAssign (nameTrg "a") IntTy (constIntExp 0),
                For
                  (nameTrg "i")
                  (call (name "f") [name "a"])
                  [ AugAssign (nameTrg "a") Add (name "i")
                  ]
              ]
      let expected = True
      hasAssignmentToLoopIterators prog `shouldBe` expected
  describe "hasReturnInLoops" $ do
    it "works" $ do
      let prog =
            toplevelMainDef
              [ AnnAssign (nameTrg "a") (ListTy IntTy) (call (name "range") [constIntExp 10]),
                For
                  (nameTrg "i")
                  (name "a")
                  [ Return (constIntExp 0)
                  ]
              ]
      let expected = True
      hasReturnInLoops prog `shouldBe` expected
  describe "hasMixedAssignment" $ do
    it "works" $ do
      let prog =
            toplevelMainDef
              [ AnnAssign (tupleTrg [nameTrg "a", subscriptTrg (nameTrg "b") (constIntExp 0)]) (ListTy IntTy) (call (name "range") [constIntExp 10])
              ]
      let expected = True
      hasMixedAssignment prog `shouldBe` expected
