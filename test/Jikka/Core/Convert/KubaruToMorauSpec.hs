{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.KubaruToMorauSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.ConstantFolding as ConstantFolding
import Jikka.Core.Convert.KubaruToMorau (run)
import qualified Jikka.Core.Convert.TypeInfer as TypeInfer
import Jikka.Core.Format (formatProgram)
import Jikka.Core.Language.Expr
import Jikka.Core.Parse (parseProgram)
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . (ConstantFolding.run <=< run)

parseProgram' :: [String] -> Program
parseProgram' = fromSuccess . flip evalAlphaT 100 . (TypeInfer.run <=< parseProgram . unlines)

spec :: Spec
spec = describe "run" $ do
  it "works on simple kubaru-style DP" $ do
    let prog =
          parseProgram'
            [ "let k: int = 1000",
              "in let dp1: int list = range k",
              "in foldl (fun dp2 i ->",
              "    foldl (fun dp3 j ->",
              "        dp3[i + j + 1 <- dp3[i + j + 1] + dp3[i]]",
              "    ) dp2 (range (k - i - 1))",
              ") dp1 (range k)"
            ]
    let expected =
          parseProgram'
            [ "let k: int = 1000",
              "in let dp1: int list = range k",
              "in build (fun dp3 ->",
              "    foldl (fun $1 i ->",
              "        $1 + dp3[i]",
              "    ) dp1[len dp3] (range (len dp3))",
              ") nil k"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
