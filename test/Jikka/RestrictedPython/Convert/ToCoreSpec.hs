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
              "    in let $4: ($5 * $6) =",
              "        foldl((fun ($4: ($5 * $6)) ($3: $2) ->",
              "            let b: $5 =",
              "                proj0($4)",
              "            in let a: $6 =",
              "                proj1($4)",
              "            in let i: $7 =",
              "                $3",
              "            in let c: $8 =",
              "                (a + b)",
              "            in let a: $9 =",
              "                b",
              "            in let b: $10 =",
              "                c",
              "            in tuple(b, a)",
              "        ), tuple(b, a), range1(n))",
              "    in let b: $5 =",
              "        proj0($4)",
              "    in let a: $6 =",
              "        proj1($4)",
              "    in a",
              "in",
              "solve"
            ]
    (Y.Format.run' <$> run' prog) `shouldBe` Right expected
  it "converts if-statements correctly" $ do
    let prog =
          [ X.ToplevelFunctionDef
              "solve"
              []
              X.IntTy
              [ X.If
                  (X.constBoolExp True)
                  [ X.AnnAssign (X.NameTrg "x") X.IntTy (X.constIntExp 1)
                  ]
                  [ X.AnnAssign (X.NameTrg "x") X.IntTy (X.constIntExp 0)
                  ],
                X.Return (X.Name "x")
              ]
          ]
    let expected =
          unlines
            [ "let rec solve : int =",
              "    let $2: ($1,) =",
              "        (if true then let x: $3 =",
              "            1",
              "        in tuple(x) else let x: $5 =",
              "            0",
              "        in tuple(x))",
              "    in let x: $1 =",
              "        proj0($2)",
              "    in x",
              "in",
              "solve"
            ]
    (Y.Format.run' <$> run' prog) `shouldBe` Right expected
