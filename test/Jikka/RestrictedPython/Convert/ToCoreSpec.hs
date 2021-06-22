{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.ToCoreSpec (spec) where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Format as Y.Format
import qualified Jikka.Core.Language.BuiltinPatterns as Y
import qualified Jikka.Core.Language.Expr as Y
import Jikka.RestrictedPython.Convert.ToCore (run)
import qualified Jikka.RestrictedPython.Language.Expr as X
import qualified Jikka.RestrictedPython.Language.Util as X
import Test.Hspec

run' :: X.Program -> Either Error Y.Program
run' = flip evalAlphaT 0 . run

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          [ X.ToplevelFunctionDef
              "solve"
              [("n", X.IntTy)]
              X.IntTy
              [ X.If
                  (X.Compare (X.Name "n") (X.CmpOp' X.Eq' X.IntTy) (X.constIntExp 0))
                  [ X.Return (X.constIntExp 1)
                  ]
                  [ X.Return (X.BinOp (X.Name "n") X.Mult (X.Call (X.Name "solve") [X.BinOp (X.Name "n") X.Sub (X.constIntExp 1)]))
                  ]
              ]
          ]
    let expected =
          Y.ToplevelLetRec
            "solve"
            [("n", Y.IntTy)]
            Y.IntTy
            ( Y.If'
                (Y.VarTy "$0")
                (Y.Equal' Y.IntTy (Y.Var "n") Y.Lit0)
                Y.Lit1
                ( Y.Mult'
                    (Y.Var "n")
                    (Y.App (Y.Var "solve") [Y.Minus' (Y.Var "n") Y.Lit1])
                )
            )
            (Y.ResultExpr (Y.Var "solve"))
    run' prog `shouldBe` Right expected
  it "converts for-loops to foldl" $ do
    let prog =
          [ X.ToplevelFunctionDef
              "solve"
              [("n", X.IntTy)]
              X.IntTy
              [ X.AnnAssign (X.NameTrg "a") X.IntTy (X.constIntExp 0),
                X.AnnAssign (X.NameTrg "b") X.IntTy (X.constIntExp 1),
                X.For
                  (X.NameTrg "i")
                  (X.Call (X.Constant $ X.ConstBuiltin X.BuiltinRange1) [X.Name "n"])
                  [ X.AnnAssign (X.NameTrg "c") X.IntTy (X.BinOp (X.Name "a") X.Add (X.Name "b")),
                    X.AnnAssign (X.NameTrg "a") X.IntTy (X.Name "b"),
                    X.AnnAssign (X.NameTrg "b") X.IntTy (X.Name "c")
                  ],
                X.Return (X.Name "a")
              ]
          ]
    let expected =
          unlines
            [ "let rec solve (n: int): int =",
              "    let a: $0 =",
              "        0",
              "    in let b: $1 =",
              "        1",
              "    in let $3: ($4 * $5) =",
              "        foldl((fun ($3: ($4 * $5)) ->",
              "            let b: $4 =",
              "                proj0($3)",
              "            in let a: $5 =",
              "                proj1($3)",
              "            in let i: $6 =",
              "                $3",
              "            in let c: $7 =",
              "                (a + b)",
              "            in let a: $8 =",
              "                b",
              "            in let b: $9 =",
              "                c",
              "            in tuple(b, a)",
              "        ), tuple(b, a), range1(n))",
              "    in let b: $4 =",
              "        proj0($3)",
              "    in let a: $5 =",
              "        proj1($3)",
              "    in a",
              "in",
              "solve"
            ]
    (Y.Format.run' <$> run' prog) `shouldBe` Right expected
