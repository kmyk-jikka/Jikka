{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.CloseMinSpec (spec) where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.CloseMin (rule)
import qualified Jikka.Core.Convert.TypeInfer as TypeInfer
import Jikka.Core.Format (formatProgram)
import Jikka.Core.Language.Expr
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Parse (parseProgram)
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . applyRewriteRuleProgram' rule

parseProgram' :: [String] -> Program
parseProgram' = fromSuccess . flip evalAlphaT 100 . (TypeInfer.run <=< parseProgram . unlines)

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          parseProgram'
            [ "fun xs ->",
              "    minimum (map (fun x -> 3 + x) xs)"
            ]
    let expected =
          parseProgram'
            [ "fun xs ->",
              "    3 + minimum (map (fun x -> x) xs)"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
  it "reduces minimum-cons if non-nil" $ do
    let prog =
          parseProgram'
            [ "fun xs ->",
              "    assert xs /= nil",
              "    in minimum (cons 0 xs)"
            ]
    let expected =
          parseProgram'
            [ "fun xs ->",
              "    assert xs /= nil",
              "    in min 0 (minimum xs)"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
  it "reduces maximum-cons if non-nil" $ do
    let prog =
          parseProgram'
            [ "fun xs ->",
              "    assert 3 - len xs <= 0", -- Jikka.Core.Language.AssertedHint assumes exprs are converted with Jikka.Core.Convert.EqualitySolving.
              "    in maximum (cons 0 xs)"
            ]
    let expected =
          parseProgram'
            [ "fun xs ->",
              "    assert 3 - len xs <= 0",
              "    in max 0 (maximum xs)"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
  it "doesn't reduce minimum-cons if it may be nil" $ do
    let prog =
          parseProgram'
            [ "fun xs ->",
              "    minimum (cons 0 xs)"
            ]
    let expected =
          parseProgram'
            [ "fun xs ->",
              "    minimum (cons 0 xs)"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
