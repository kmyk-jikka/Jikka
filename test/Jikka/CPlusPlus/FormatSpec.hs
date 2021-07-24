module Jikka.CPlusPlus.FormatSpec
  ( spec,
  )
where

import Data.List
import Jikka.CPlusPlus.Format
import Jikka.CPlusPlus.Language.Expr
import Test.Hspec

run'' :: Program -> [String]
run'' prog = dropWhile ("#include" `isPrefixOf`) (lines (run' prog))

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let program =
          Program
            [ FunDef
                TyInt64
                (VarName "solve")
                [(TyInt32, VarName "n")]
                [ Declare TyInt64 (VarName "x") (Just (Lit (LitInt64 0))),
                  For
                    TyInt32
                    (VarName "i")
                    (Lit (LitInt32 0))
                    (BinOp LessThan (Var (VarName "i")) (Var (VarName "n")))
                    (AssignIncr (LeftVar (VarName "i")))
                    [ Assign (AssignExpr AddAssign (LeftVar (VarName "x")) (Call (Cast TyInt64) [Var (VarName "i")]))
                    ],
                  Return (Var (VarName "x"))
                ]
            ]
    let formatted =
          [ "int64_t solve(int32_t n) {",
            "    int64_t x = 0;",
            "    for (int32_t i = 0; i < n; ++ i) {",
            "        x += int64_t(i);",
            "    }",
            "    return x;",
            "}"
          ]
    run'' program `shouldBe` formatted
