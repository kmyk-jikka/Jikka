module Jikka.Serializer.CPlusPlusSpec
  ( spec,
  )
where

import Jikka.Language.CPlusPlus.Expr
import Jikka.Serializer.CPlusPlus
import Test.Hspec

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
                    [ Assign (AssignExpr AddAssign (LeftVar (VarName "x")) (Cast TyInt64 (Var (VarName "i"))))
                    ],
                  Return (Var (VarName "x"))
                ]
            ]
    let formatted =
          unlines
            [ "int64_t solve(int32_t n) {",
              "    int64_t x = 0;",
              "    for (int32_t i = 0; i < n; ++ i) {",
              "        x += static_cast<int64_t>(i);",
              "    }",
              "    return x;",
              "}"
            ]
    run' program `shouldBe` formatted
