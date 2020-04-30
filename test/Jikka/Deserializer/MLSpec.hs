{-# LANGUAGE OverloadedStrings #-}

module Jikka.Deserializer.MLSpec
  ( spec,
  )
where

import qualified Data.Map as M
import Data.Text (Text, pack)
import Jikka.Deserializer.ML
import Jikka.Language.Type
import qualified Jikka.Serializer.Eval as Eval
import Test.Hspec

run' :: String -> Either String Program
run' = run "*test*" . pack

spec :: Spec
spec = do
  specSimple
  specEval

specSimple :: Spec
specSimple = describe "deserializer (simple)" $ do
  it "let rec function" $ do
    let input =
          concat
            [ "let given n : int in\n",
              "let rec fact = function\n",
              "    | 0 -> 1\n",
              "    | n + 1 -> (n + 1) * fact n\n",
              "    end in\n",
              "fact n\n"
            ]
    let given = [("n", TyInt)]
    let body =
          Let
            "fact"
            (TyVar "_1")
            ( App
                ( Fun
                    (Rec "fact")
                    [ ( [PatLit Unit],
                        Fun
                          NoRec
                          [ ([PatLit (Int 0)], Lit (Int 1)),
                            ([PatPlusK "n" 1], App (App (BuiltIn Mul) (App (App (BuiltIn Add) (Var "n")) (Lit (Int 1)))) (App (Var "fact") (Var "n")))
                          ]
                      )
                    ]
                )
                (Lit Unit)
            )
            (App (Var "fact") (Var "n"))
    let prog = Program given body
    run' input `shouldBe` Right prog
  it "let rec match" $ do
    let input =
          concat
            [ "let given n : int in\n",
              "let rec fact n = match n with\n",
              "    | 0 -> 1\n",
              "    | n + 1 -> (n + 1) * fact n\n",
              "    end in\n",
              "fact n\n"
            ]
    let given = [("n", TyInt)]
    let fun =
          Fun
            NoRec
            [ ([PatLit (Int 0)], Lit (Int 1)),
              ([PatPlusK "n" 1], App (App (BuiltIn Mul) (App (App (BuiltIn Add) (Var "n")) (Lit (Int 1)))) (App (Var "fact") (Var "n")))
            ]
    let body =
          Let
            "fact"
            (TyFun (TyVar "_1") (TyVar "_2"))
            ( Fun
                (Rec "fact")
                [ ([PatVar "n"], App fun (Var "n"))
                ]
            )
            (App (Var "fact") (Var "n"))
    let prog = Program given body
    run' input `shouldBe` Right prog

run'' :: String -> Either String Expr
run'' input = do
  prog <- run "*test*" (pack input)
  case given prog of
    [] -> Eval.run' M.empty (body prog)
    _ -> Left "Runtime Error: given should be nothing"

specEval :: Spec
specEval = describe "deserializer (eval)" $ do
  it "let rec function" $ do
    let input =
          concat
            [ "let rec fact = function\n",
              "    | 0 -> 1\n",
              "    | n + 1 -> (n + 1) * fact n\n",
              "    end in\n",
              "fact 10\n"
            ]
    let value = Lit . Int $ product [1 .. 10]
    run'' input `shouldBe` Right value
  it "let rec match" $ do
    let input =
          concat
            [ "let rec fact n = match n with\n",
              "    | 0 -> 1\n",
              "    | n + 1 -> (n + 1) * fact n\n",
              "    end in\n",
              "fact 10\n"
            ]
    let value = Lit . Int $ product [1 .. 10]
    run'' input `shouldBe` Right value
