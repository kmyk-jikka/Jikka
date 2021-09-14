{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.EtaSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.Eta (run)
import qualified Jikka.Core.Convert.TypeInfer as TypeInfer
import Jikka.Core.Language.Expr
import Jikka.Core.Parse (parseProgram)
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

parseProgram' :: [String] -> Program
parseProgram' = fromSuccess . flip evalAlphaT 100 . (TypeInfer.run <=< parseProgram . unlines)

spec :: Spec
spec = describe "run" $ do
  it "works on a let" $ do
    let prog =
          parseProgram'
            [ "fun (n: int) ->",
              "    let f = sum",
              "    in f"
            ]
    let expected =
          parseProgram'
            [ "fun (n: int) ->",
              "    let f = fun $0 -> sum $0",
              "    in f"
            ]
    run' prog `shouldBe` Right expected
  it "works in map" $ do
    let prog =
          parseProgram'
            [ "map sum nil"
            ]
    let expected =
          parseProgram'
            [ "map (fun $0 -> sum $0) nil"
            ]
    run' prog `shouldBe` Right expected
