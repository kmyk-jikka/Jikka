{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.TypeInferSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.TypeInfer (run, runExpr)
import Jikka.Core.Language.Expr
import Jikka.Core.Parse (parseExpr, parseProgram)
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

runExpr' :: [(VarName, Type)] -> Expr -> Either Error Expr
runExpr' env = flip evalAlphaT 0 . runExpr env

parseExpr' :: [String] -> Expr
parseExpr' = fromSuccess . flip evalAlphaT 100 . parseExpr . unlines

parseProgram' :: [String] -> Program
parseProgram' = fromSuccess . flip evalAlphaT 100 . parseProgram . unlines

spec :: Spec
spec = do
  describe "run" $ do
    it "works" $ do
      let prog =
            parseProgram'
              [ "let x: t1 = 0",
                "in let y: t2 = x + 1",
                "in y"
              ]
      let expected =
            parseProgram'
              [ "let x: int = 0",
                "in let y: int = x + 1",
                "in y"
              ]
      run' prog `shouldBe` Right expected
    it "works on let-rec" $ do
      let prog =
            parseProgram'
              [ "let rec f (x: t1): t2 = x",
                "in f 0"
              ]
      let expected =
            parseProgram'
              [ "let rec f (x: int): int = x",
                "in f 0"
              ]
      run' prog `shouldBe` Right expected
    it "replaces undetermined types with 0-tuples" $ do
      let prog =
            parseProgram'
              [ "let rec f (x: t1): t2 = x",
                "in 0"
              ]
      let expected =
            parseProgram'
              [ "let rec f (x: unit): unit = x",
                "in 0"
              ]
      run' prog `shouldBe` Right expected
    it "works on builtin functions" $ do
      let prog =
            parseProgram'
              [ "let rec solve (n: t0): t1 =",
                "    if@t2 n ==@t3 0",
                "    then 1",
                "    else n * solve (n - 1)",
                "in solve"
              ]
      let expected =
            parseProgram'
              [ "let rec solve (n: int): int =",
                "    if@int n ==@int 0",
                "    then 1",
                "    else n * solve (n - 1)",
                "in solve"
              ]
      run' prog `shouldBe` Right expected

  describe "runExpr" $ do
    it "works on an equation" $ do
      let env = [("xs", ListTy BoolTy)]
      let e =
            parseExpr'
              [ "xs ==@t1 nil@t2"
              ]
      let expected =
            parseExpr'
              [ "xs ==@(bool list) nil@bool"
              ]
      runExpr' env e `shouldBe` Right expected

    it "works on lambdas and applications" $ do
      let env = [("f", Fun3Ty IntTy BoolTy UnitTy (ListTy IntTy))]
      let e =
            parseExpr'
              [ "fun x y z -> f x y z"
              ]
      let expected =
            parseExpr'
              [ "fun (x: int) (y: bool) (z: unit) -> f x y z"
              ]
      runExpr' env e `shouldBe` Right expected
