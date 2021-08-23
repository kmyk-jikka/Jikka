{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.SpecializeFoldlSpec (spec) where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.SpecializeFoldl (run)
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
  it "works about sum" $ do
    let prog =
          parseProgram'
            [ "fun n ->",
              "    foldl (fun y x -> y + x) 12 (range n)"
            ]
    let expected =
          parseProgram'
            [ "fun n ->",
              "    12 + sum (map (fun x -> x) (range n))"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
  it "works about complicated sum" $ do
    let prog =
          parseProgram'
            [ "fun n ->",
              "    foldl (fun y x -> 1 + 2 + y + x + 3) 0 (range n)"
            ]
    let expected =
          parseProgram'
            [ "fun n ->",
              "    0 + sum (map (fun x -> x + 6) (range n))"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
  it "works about modsum" $ do
    let prog =
          parseProgram'
            [ "fun n ->",
              "    foldl (fun y x -> modplus y x 1000000007) 0 (range n)"
            ]
    let expected =
          parseProgram'
            [ "fun n ->",
              "    modplus 0 (modsum (map (fun x -> x) (range n)) 1000000007) 1000000007"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
  it "works about modsum with wrapping" $ do
    let prog =
          parseProgram'
            [ "fun n init ->",
              "    foldl (fun y x -> y % 3) init (range n)"
            ]
    let expected =
          parseProgram'
            [ "fun n init ->",
              "    if (range n) == nil",
              "    then init",
              "    else modplus init (modsum (map (fun x -> 0) (range n)) 3) 3"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
