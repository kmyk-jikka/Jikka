{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.SortAbsSpec (spec) where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.SortAbs (run)
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
            [ "fun (a: int list) ->",
              "    sum (map (fun (a_i: int) ->",
              "        sum (map (fun (a_j: int) ->",
              "            abs (a_i - a_j)",
              "        ) a)",
              "    ) a)"
            ]
    let expected =
          parseProgram'
            [ "fun (a: int list) ->",
              "    let a$6 = sorted a",
              "    in sum (map (fun ($7: int) ->",
              "        let a_i$8 = a$6[$7] in",
              "        sum (map (fun ($9: int) ->",
              "            let a_j$10 = a$6[$9]",
              "            in a_i$8 - a_j$10",
              "        ) (range $7))",
              "        + (let $11 = $7",
              "           in let a_j$12 = a$6[$11]",
              "           in 0)",
              "        + sum (map (fun ($13: int) ->",
              "            let a_j$14 = a$6[$13]",
              "            in a_j$14 - a_i$8",
              "        ) (range2 ($7 + 1) (len a$6)))",
              "    ) (range (len a$6)))"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
