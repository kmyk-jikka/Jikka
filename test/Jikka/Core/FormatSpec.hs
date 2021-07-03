{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.FormatSpec
  ( spec,
  )
where

import Jikka.Core.Format
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let program =
          ToplevelLetRec
            "solve$0"
            [("n$1", IntTy)]
            IntTy
            ( Let
                "xs$2"
                (ListTy IntTy)
                ( Tabulate'
                    IntTy
                    (Var "n$1")
                    ( Lam
                        [("i$3", IntTy)]
                        (Mult' (Var "i$3") (Var "i$3"))
                    )
                )
                (Sum' (Var "xs$2"))
            )
            (ResultExpr (Var "solve$0"))
    let expected =
          unlines
            [ "let rec solve$0 (n$1: int): int =",
              "    let xs$2: int list =",
              "        tabulate(n$1, (fun (i$3: int) ->",
              "            (i$3 * i$3)",
              "        ))",
              "    in sum(xs$2)",
              "in",
              "solve$0"
            ]
    run' program `shouldBe` expected
