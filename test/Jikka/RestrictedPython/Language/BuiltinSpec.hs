{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Language.BuiltinSpec
  ( spec,
  )
where

import qualified Data.Set as S
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Builtin
import Jikka.RestrictedPython.Language.Expr
import Test.Hspec

resolveBuiltin' :: VarName' -> Int -> Either Error Expr'
resolveBuiltin' x n = flip evalAlphaT 0 $ resolveBuiltin x n

spec :: Spec
spec = describe "resolveBuiltin" $ do
  it "works" $ do
    let f = Right . withoutLoc . Constant . ConstBuiltin
    resolveBuiltin' (withoutLoc "max") 1 `shouldBe` f (BuiltinMax1 (VarTy "$0"))
    resolveBuiltin' (withoutLoc "max") 2 `shouldBe` f (BuiltinMax (VarTy "$0") 2)
    resolveBuiltin' (withoutLoc "mox") 2 `shouldBe` Right (withoutLoc (Name "mox"))
  it "is exhaustive" $ do
    let resolve x =
          let f n = resolveBuiltin' (withoutLoc x) n
           in map f [0 .. 4]
    let isBuiltin = \case
          Left _ -> False
          Right x -> case value' x of
            Constant (ConstBuiltin _) -> True
            _ -> False
    resolve "foo" `shouldNotSatisfy` any isBuiltin
    resolve "bar" `shouldNotSatisfy` any isBuiltin
    resolve "sum" `shouldSatisfy` any isBuiltin
    resolve "max" `shouldSatisfy` any isBuiltin
    forM_ (S.toList builtinNames) $ \x -> do
      resolve x `shouldSatisfy` any isBuiltin
