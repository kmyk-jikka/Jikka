{-# LANGUAGE OverloadedStrings #-}

module Jikka.Python.Convert.ToRestrictedPythonSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.Location
import Jikka.Python.Convert.ToRestrictedPython (run)
import qualified Jikka.Python.Language.Expr as X
import qualified Jikka.RestrictedPython.Language.Expr as Y
import Test.Hspec

at :: a -> Int -> WithLoc a
at a x = WithLoc (Loc 0 x (-1)) a

at' :: a -> Int -> WithLoc' a
at' a x = WithLoc' (Just (Loc 0 x (-1))) a

run' :: X.Program -> Either Error Y.Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let parsed =
          [ X.FunctionDef
              ("solve" `at` 6)
              (X.emptyArguments {X.argsArgs = [("x" `at` 7, Nothing)]})
              [ X.Assign [X.Name ("y" `at` 8) `at` 5] (X.Name ("x" `at` 3) `at` 3) `at` 4
              ]
              []
              (Just (X.Name ("int" `at` 2) `at` 2))
              `at` 1
          ]
    let expected =
          [ Y.ToplevelFunctionDef
              ("solve" `at'` 6)
              [("x" `at'` 7, Y.VarTy "$0")]
              Y.IntTy
              [ Y.AnnAssign (Y.NameTrg ("y" `at'` 8)) (Y.VarTy "$1") (Y.Name ("x" `at'` 3))
              ]
          ]
    run' parsed `shouldBe` Right expected
  it "works on recursive functions" $ do
    let parsed =
          [ X.FunctionDef
              ("f" `at` 6)
              (X.emptyArguments {X.argsArgs = [("x" `at` 7, Nothing)]})
              [ X.Return (Just (X.Call (X.Name ("f" `at` 8) `at` 5) [X.Name ("x" `at` 4) `at` 4] [] `at` 3)) `at` 2
              ]
              []
              Nothing
              `at` 1
          ]
    let expected =
          [ Y.ToplevelFunctionDef
              ("f" `at'` 6)
              [("x" `at'` 7, Y.VarTy "$0")]
              (Y.VarTy "$1")
              [ Y.Return (Y.Call (Y.Name ("f" `at'` 8)) [Y.Name ("x" `at'` 4)])
              ]
          ]
    run' parsed `shouldBe` Right expected
