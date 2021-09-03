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
              "    let a$3 = sorted a",
              "    in sum (map (fun ($4: int) ->",
              "        let a_i = a$3[$4] in",
              "        sum (map (fun ($5: int) ->",
              "            let a_j = a$3[$5]",
              "            in a_i - a_j",
              "        ) (range $4))",
              "        + (let $6 = $4",
              "           in let a_j$7 = a$3[$6]",
              "           in 0)",
              "        + sum (map (fun ($8: int) ->",
              "            let a_j$9 = a$3[$8]",
              "            in a_j$9 - a_i",
              "        ) (range2 ($4 + 1) (len a$3)))",
              "    ) (range (len a$3)))"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
