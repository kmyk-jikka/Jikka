{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.CumulativeSumSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.CumulativeSum (run)
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
  it "works" $ do
    let prog =
          parseProgram'
            [ "let rec f: int =", -- Remove distinction between top-level and non-top-level.
              "    let a: int list = range 1000",
              "    in let n: int = 500",
              "    in sum (map (fun i -> a[i]) (range n))",
              "in f"
            ]
    let expected =
          parseProgram'
            [ "let rec f$0: int =",
              "    let a$1: int list = range 1000",
              "    in let n$2: int = 500",
              "    in let $4 = scanl (+) 0 a$1",
              "    in $4[n$2]",
              "in f$0"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
