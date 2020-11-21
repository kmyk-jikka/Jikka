module Jikka.Converter.Python.ConvertSpec (spec) where

import Data.Either (isLeft)
import Jikka.Converter.Python.Convert (run)
import Jikka.Language.Common.Name
import Jikka.Language.Common.Pos
import qualified Jikka.Language.Python.Parsed.Type as X
import qualified Jikka.Language.Python.Typed.Stdlib as Y
import qualified Jikka.Language.Python.Typed.Type as Y
import Test.Hspec

at :: a -> Int -> WithPos a
at a x = WithPos (Pos 0 x) a

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
                [(VarName "x@1", Y.ATyVar (TypeName "u@0"))]
                Y.ATyNat
                [ Y.Define (VarName "y@2") (Y.ATyVar (TypeName "u@1")) (Y.Var (VarName "x@1"))
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
                [(VarName "x@1", Y.ATyVar (TypeName "u@0"))]
                (Y.ATyVar (TypeName "u@1"))
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
