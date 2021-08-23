{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.PropagateModSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.PropagateMod (run)
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
  it "works over a function" $ do
    let prog =
          parseProgram'
            [ "fun y ->",
              "    (fun x -> x * x + x) y % 1000000007"
            ]
    let expected =
          parseProgram'
            [ "fun y ->",
              "    (fun x$0 -> modplus (x$0 % 1000000007) (modmult (x$0 % 1000000007) (x$0 % 1000000007) 1000000007) 1000000007) y"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
  it "works with sum" $ do
    let prog =
          parseProgram'
            [ "fun xs ->",
              "    sum (map (fun x -> 2 * x) xs) % 1000000007"
            ]
    let expected =
          parseProgram'
            [ "fun xs ->",
              "    modsum (map (fun x$0 -> modmult (x$0 % 1000000007) 2 1000000007) xs) 1000000007"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
