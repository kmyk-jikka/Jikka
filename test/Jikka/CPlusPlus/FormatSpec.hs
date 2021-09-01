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
spec = do
  describe "run" $ do
    it "works" $ do
      let program =
            Program
              [ FunDef
                  TyInt64
                  (VarName "solve")
                  [(TyInt32, VarName "n")]
                  [ Declare TyInt64 (VarName "x") (DeclareCopy (Lit (LitInt64 0))),
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
              "    int64_t x = 0ll;",
              "    for (int32_t i = 0; i < n; ++ i) {",
              "        x += int64_t(i);",
              "    }",
              "    return x;",
              "}"
            ]
      run'' program `shouldBe` formatted
  describe "no unnecessary paren in index" $ do
    it "works" $ do
      let program =
            Program
              [ FunDef
                  TyInt64
                  (VarName "solve")
                  [(TyInt32, VarName "n"), (TyVector TyInt64, VarName "h")]
                  [ Declare TyInt64 (VarName "x") (DeclareCopy (Lit (LitInt64 0))),
                    For
                      TyInt32
                      (VarName "i")
                      (Lit (LitInt32 2))
                      (BinOp LessThan (Var (VarName "i")) (Var (VarName "n")))
                      (AssignIncr (LeftVar (VarName "i")))
                      [ Assign (AssignExpr AddAssign (LeftVar (VarName "x")) (Call At [Var (VarName "h"), BinOp Sub (Var (VarName "i")) (Lit (LitInt32 2))]))
                      ],
                    Return (Var (VarName "x"))
                  ]
              ]
      let formatted =
            [ "int64_t solve(int32_t n, std::vector<int64_t> h) {",
              "    int64_t x = 0ll;",
              "    for (int32_t i = 2; i < n; ++ i) {",
              "        x += h[i - 2];",
              "    }",
              "    return x;",
              "}"
            ]
      run'' program `shouldBe` formatted
