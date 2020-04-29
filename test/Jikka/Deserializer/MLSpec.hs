{-# LANGUAGE OverloadedStrings #-}

module Jikka.Deserializer.MLSpec
  ( spec,
  )
where

import Data.Text (Text, pack)
import Jikka.Deserializer.ML
import Jikka.Language.Type
import Test.Hspec

run' :: String -> Either String Expr
run' = run "*test*" . pack

spec :: Spec
spec = describe "deserializer" $ do
  it "let rec function" $ do
    let input =
          concat
            [ "let rec fact = function\n",
              "    | 0 -> 0\n",
              "    | n + 1 -> (n + 1) * fact n\n",
              "    end\n",
              "in fact 10\n"
            ]
    let tree =
          Let
            "fact"
            (TyVar "_1")
            ( Fun
                NoRec
                [ ([PatLit (Int 0)], Lit (Int 0)),
                  ([PatPlusK "n" 1], App (App (BuiltIn Mul) (App (App (BuiltIn Add) (Var "n")) (Lit (Int 1)))) (App (Var "fact") (Var "n")))
                ]
            )
            (App (Var "fact") (Lit (Int 10)))
    run' input `shouldBe` Right tree
  it "let rec match" $ do
    let input =
          concat
            [ "let rec fact n = match n with\n",
              "    | 0 -> 0\n",
              "    | n + 1 -> (n + 1) * fact n\n",
              "    end\n",
              "in fact 10\n"
            ]
    let fun =
          Fun
            NoRec
            [ ([PatLit (Int 0)], Lit (Int 0)),
              ([PatPlusK "n" 1], App (App (BuiltIn Mul) (App (App (BuiltIn Add) (Var "n")) (Lit (Int 1)))) (App (Var "fact") (Var "n")))
            ]
    let tree =
          Let
            "fact"
            (TyFun (TyVar "_1") (TyVar "_2"))
            ( Fun
                (Rec "fact")
                [ ([PatVar "n"], App fun (Var "n"))
                ]
            )
            (App (Var "fact") (Lit (Int 10)))
    run' input `shouldBe` Right tree
