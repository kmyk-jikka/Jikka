{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.ParseSpec
  ( spec,
  )
where

import qualified Data.Text as T
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Parse
import Test.Hspec

run' :: String -> Either Error Program
run' prog = evalAlphaT (run "<data>" (T.pack prog)) 100

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          unlines
            [ "let rec solve$0 (n$1: int): int =",
              "    let xs$2: int list =",
              "        map (fun (i$3: int) ->",
              "            i$3 * i$3",
              "        ) (range n$1)",
              "    in sum xs$2",
              "in",
              "solve$0"
            ]
    let expected =
          ToplevelLetRec
            "solve$0"
            [("n$1", IntTy)]
            IntTy
            ( Let
                "xs$2"
                (ListTy IntTy)
                ( Map'
                    (VarTy "$100")
                    (VarTy "$101")
                    ( Lam
                        "i$3"
                        IntTy
                        (Mult' (Var "i$3") (Var "i$3"))
                    )
                    (Range1' (Var "n$1"))
                )
                (Sum' (Var "xs$2"))
            )
            (ResultExpr (Var "solve$0"))
    run' prog `shouldBe` Right expected
  it "inserts new type variables" $ do
    let prog = "a[0 <- b][0]"
    let expected =
          ResultExpr
            (At' (VarTy "$100") (SetAt' (VarTy "$101") (Var "a") (LitInt' 0) (Var "b")) (LitInt' 0))
    run' prog `shouldBe` Right expected
