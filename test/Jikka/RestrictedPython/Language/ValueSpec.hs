module Jikka.RestrictedPython.Language.ValueSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Jikka.RestrictedPython.Language.Stdlib as Stdlib
import Jikka.RestrictedPython.Language.Value
import Test.Hspec

spec :: Spec
spec = do
  describe "standardBuiltinFunctions" $ do
    it "matches Jikka.RestrictedPython.Language.Stdlib.standardBuiltinFunctions" $ do
      M.keysSet standardBuiltinFunctions `shouldBe` Stdlib.standardBuiltinFunctions
  describe "additionalBuiltinFunctions" $ do
    it "matches Jikka.RestrictedPython.Language.Stdlib.additionalBuiltinFunctions" $ do
      M.keysSet additionalBuiltinFunctions `shouldBe` Stdlib.additionalBuiltinFunctions
