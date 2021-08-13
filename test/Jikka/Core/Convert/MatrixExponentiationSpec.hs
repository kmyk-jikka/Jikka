{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.MatrixExponentiationSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.MatrixExponentiation (run)
import qualified Jikka.Core.Convert.TypeInfer as TypeInfer
import Jikka.Core.Format (formatProgram)
import Jikka.Core.Language.Expr
import Jikka.Core.Parse (parseProgram)
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

parseProgram' :: [String] -> Program
parseProgram' = fromSuccess . flip evalAlphaT 100 . (TypeInfer.run <=< parseProgram . unlines)

spec :: Spec
spec = describe "run" $ do
  it "works about matrices" $ do
    let prog =
          parseProgram'
            [ "let c: int = 10",
              "in let k: int = 1000",
              "in iterate k (fun x -> (x.0 + c * x.1, x.0)) (12, 34)"
            ]
    let expected =
          parseProgram'
            [ "let c: int = 10",
              "in let k: int = 1000",
              "in matap@2@2 (matpow@2 ((1, c), (1, 0)) k) (12, 34)"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
  it "works about integers" $ do
    let prog =
          parseProgram'
            [ "let c: int = 10",
              "in let k: int = 1000",
              "in iterate k (fun x -> c * x + 2) 1234"
            ]
    let expected =
          parseProgram'
            [ "let c: int = 10",
              "in let k: int = 1000",
              "in c ** k * 1234 + (c ** k - 1) / (c - 1) * 2"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
