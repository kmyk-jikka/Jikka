module Jikka.Language.Python.Typed.TypeInferSpec
  ( spec,
  )
where

import Data.Either (isLeft)
import Jikka.Language.Common.Name
import Jikka.Language.Python.Typed.Expr
import Jikka.Language.Python.Typed.Stdlib
import Jikka.Language.Python.Typed.TypeInfer
import Test.Hspec

spec :: Spec
spec = do
  describe "infer'" $ do
    it "works" $ do
      let expr = UnOp Abs (Lit (LitInt (-10)))
      let expected = ATyNat
      infer' emptyTypeEnv expr `shouldBe` Right expected
  describe "check" $ do
    it "works" $ do
      let expr = UnOp Range1 (Lit (LitInt 10))
      let expected = TyIterator TyInt
      check emptyTypeEnv expr `shouldBe` Right expected
    it "works'" $ do
      let expr = BinOp Plus (Lit (LitInt 0)) (Lit (LitBool False))
      check emptyTypeEnv expr `shouldSatisfy` isLeft
