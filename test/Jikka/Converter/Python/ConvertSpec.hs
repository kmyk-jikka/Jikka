module Jikka.Converter.Python.ConvertSpec (spec) where

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
