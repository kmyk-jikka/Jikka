module Jikka.Python.Convert.ToRestrictedPythonSpec (spec) where

import Data.Either (isLeft)
import Jikka.Common.Language.Name
import Jikka.Common.Location
import Jikka.Python.Convert.ToRestrictedPython (run)
import qualified Jikka.Python.Language.Expr as X
import qualified Jikka.RestrictedPython.Language.Expr as Y
import qualified Jikka.RestrictedPython.Language.Stdlib as Y
import Test.Hspec

at :: a -> Int -> WithLoc a
at a x = WithLoc (Loc 0 x (-1)) a

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let parsed =
          X.Program
            [ X.FunDef
                (FunName "solve@0")
                [(VarName "x@1", Nothing)]
                (Just (X.TyNat `at` 2))
                [ X.Define (VarName "y@2") Nothing (X.Var (VarName "x@1") `at` 3) `at` 4
                ]
                `at` 1
            ]
    let expected =
          Y.Program
            [ Y.FunDef
                (FunName "solve@0")
                [(VarName "x@1", Y.ATyVar (TypeName "t@0"))]
                Y.ATyNat
                [ Y.Define (VarName "y@2") (Y.ATyVar (TypeName "t@1")) (Y.Var (VarName "x@1"))
                ]
            ]
    run parsed `shouldBe` Right expected
  it "works on recursive functions" $ do
    let parsed =
          X.Program
            [ X.FunDef
                (FunName "f@0")
                [(VarName "x@1", Nothing)]
                Nothing
                [ X.Return (X.Call (FunName "f@0") [X.Var (VarName "x@1") `at` 4] `at` 3) `at` 2
                ]
                `at` 1
            ]
    let expected =
          Y.Program
            [ Y.FunDef
                (FunName "f@0")
                [(VarName "x@1", Y.ATyVar (TypeName "t@0"))]
                (Y.ATyVar (TypeName "t@1"))
                [ Y.Return (Y.Call (FunName "f@0") [Y.Var (VarName "x@1")])
                ]
            ]
    run parsed `shouldBe` Right expected
  it "fails when a function is called with the invalid number of arguments" $ do
    let parsed =
          X.Program
            [ X.FunDef
                (FunName "f@0")
                [(VarName "x@1", Nothing)]
                Nothing
                [ X.Return (X.Call (FunName "f@0") [] `at` 3) `at` 2
                ]
                `at` 1
            ]
    run parsed `shouldSatisfy` isLeft
