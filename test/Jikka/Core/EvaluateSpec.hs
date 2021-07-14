{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.EvaluateSpec (spec) where

import qualified Data.Vector as V
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Evaluate
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Value (formatValue)
import Test.Hspec

run'' :: Program -> [Value] -> Either Error Value
run'' = (flip evalAlphaT 0 .) . run

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
    let args =
          [ ValList
              ( V.fromList
                  [ ValInt 1,
                    ValInt 2,
                    ValInt 5
                  ]
              )
          ]
    let expected = ValInt 11
    (formatValue <$> run'' prog args) `shouldBe` Right (formatValue expected)
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
    let args =
          [ ValInt 10
          ]
    let expected = ValInt 3628800
    (formatValue <$> run'' prog args) `shouldBe` Right (formatValue expected)
