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
            [ "let rec f: int =",
              "    let a: int list = range 1000",
              "    in let n: int = 500",
              "    in let $0 = scanl (+) 0 a",
              "    in $0[n]",
              "in f"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
