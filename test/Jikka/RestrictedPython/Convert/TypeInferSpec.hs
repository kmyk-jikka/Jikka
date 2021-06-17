{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.TypeInferSpec (spec) where

import qualified Data.Map.Strict as M
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Convert.TypeInfer
import Jikka.RestrictedPython.Language.Expr
import Test.Hspec

run' :: Program -> Either Error Program
run' = evalAlphaT 0 . run

spec :: Spec
spec = do
  describe "subst" $ do
    it "works" $ do
      let sigma = Subst (M.fromList [("t1", VarTy "t2"), ("t2", IntTy)])
      subst sigma (VarTy "t1") `shouldBe` IntTy
  describe "run" $ do
    it "works" $ do
        let parsed =
              [ ToplevelFunctionDef
                  "solve"
                  [("x", VarTy "t1")]
                  (VarTy "t2")
                  [ Return (BinOp (Name "x") Add (Name "x"))
                  ]
              ]
        let expected =
              [ ToplevelFunctionDef
                  "solve"
                  [("x", IntTy)]
                  IntTy
                  [ Return (BinOp (Name "x") Add (Name "x"))
                  ]
              ]
        run' parsed `shouldBe` Right expected
    it "makes undetermined type variables to the unit type" $ do
        let parsed =
              [ ToplevelFunctionDef
                  "solve"
                  [("x", VarTy "t1")]
                  (VarTy "t2")
                  [ Return (Name "x")
                  ]
              ]
        let expected =
              [ ToplevelFunctionDef
                  "solve"
                  [("x", TupleTy [])]
                  (TupleTy [])
                  [ Return (Name "x")
                  ]
              ]
        run' parsed `shouldBe` Right expected
