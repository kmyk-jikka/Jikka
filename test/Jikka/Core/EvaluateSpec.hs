{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.EvaluateSpec (spec) where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Evaluate (Token (..), Value (..), run')
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Value (formatValue)
import Test.Hspec

run'' :: [Token] -> Program -> Either Error Value
run'' = (flip evalAlphaT 0 .) . run'

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          ToplevelLetRec
            "solve"
            [("xs", ListTy IntTy)]
            IntTy
            ( Plus'
                (Sum' (Var "xs"))
                (Len' IntTy (Var "xs"))
            )
            (ResultExpr (Var "solve"))
    let tokens =
          [ Token "3",
            Token "1",
            Token "2",
            Token "5"
          ]
    let expected = ValInt 11
    (formatValue <$> run'' tokens prog) `shouldBe` Right (formatValue expected)
  it "works on a recursive function" $ do
    let prog =
          ToplevelLetRec
            "fact"
            [("n", IntTy)]
            IntTy
            ( App
                ( If'
                    (FunTy UnitTy IntTy)
                    (Equal' IntTy (Var "n") Lit0)
                    (Lam "x" UnitTy Lit1)
                    ( Lam
                        "x"
                        UnitTy
                        ( Mult'
                            (Var "n")
                            (App (Var "fact") (Minus' (Var "n") Lit1))
                        )
                    )
                )
                (Tuple' [])
            )
            (ResultExpr (Var "fact"))
    let tokens =
          [ Token "10"
          ]
    let expected = ValInt 3628800
    (formatValue <$> run'' tokens prog) `shouldBe` Right (formatValue expected)
