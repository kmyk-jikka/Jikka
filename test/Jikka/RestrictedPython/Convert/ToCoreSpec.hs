{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Convert.ToCoreSpec (spec) where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Format as Y (formatProgram)
import qualified Jikka.Core.Language.BuiltinPatterns as Y
import qualified Jikka.Core.Language.Expr as Y
import qualified Jikka.Core.Parse as Y (parseProgram)
import Jikka.RestrictedPython.Convert.ToCore (run)
import qualified Jikka.RestrictedPython.Language.Expr as X
import qualified Jikka.RestrictedPython.Language.WithoutLoc as X
import Test.Hspec

run' :: X.Program -> Either Error Y.Program
run' = flip evalAlphaT 0 . run

parseProgram :: [String] -> Y.Program
parseProgram = fromSuccess . flip evalAlphaT 100 . Y.parseProgram . unlines

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let prog =
          [ X.ToplevelFunctionDef
              "solve"
              [("n", X.IntTy)]
              X.IntTy
              [ X.If
                  (X.eqExp X.IntTy (X.name "n") (X.constIntExp 0))
                  [ X.Return (X.constIntExp 1)
                  ]
                  [ X.Return (X.binOp (X.name "n") X.Mult (X.call (X.name "solve") [X.binOp (X.name "n") X.Sub (X.constIntExp 1)]))
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
                    (Y.App (Y.Var "solve") (Y.Minus' (Y.Var "n") Y.Lit1))
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
              [ X.AnnAssign (X.nameTrg "a") X.IntTy (X.constIntExp 0),
                X.AnnAssign (X.nameTrg "b") X.IntTy (X.constIntExp 1),
                X.For
                  (X.nameTrg "i")
                  (X.call (X.constBuiltinExp X.BuiltinRange1) [X.name "n"])
                  [ X.AnnAssign (X.nameTrg "c") X.IntTy (X.binOp (X.name "a") X.Add (X.name "b")),
                    X.AnnAssign (X.nameTrg "a") X.IntTy (X.name "b"),
                    X.AnnAssign (X.nameTrg "b") X.IntTy (X.name "c")
                  ],
                X.Return (X.name "a")
              ]
          ]
    let expected =
          parseProgram
            [ "let rec solve (n: int): int =",
              "    let a: $0 = 0",
              "    in let b: $1 = 1",
              "    in let $4: $5 * $6 =",
              "        foldl (fun ($4: $5 * $6) ($3: $2) ->",
              "            let b: $5 = $4.0",
              "            in let a: $6 = $4.1",
              "            in let i: $7 = $3",
              "            in let c: $8 = a + b",
              "            in let a: $9 = b",
              "            in let b: $10 = c",
              "            in (b, a)",
              "        ) (b, a) (range n)",
              "    in let b: $5 = $4.0",
              "    in let a: $6 = $4.1",
              "    in a",
              "in",
              "solve"
            ]
    (Y.formatProgram <$> run' prog) `shouldBe` Right (Y.formatProgram expected)
  it "converts if-statements correctly" $ do
    let prog =
          [ X.ToplevelFunctionDef
              "solve"
              []
              X.IntTy
              [ X.If
                  (X.constBoolExp True)
                  [ X.AnnAssign (X.nameTrg "x") X.IntTy (X.constIntExp 1)
                  ]
                  [ X.AnnAssign (X.nameTrg "x") X.IntTy (X.constIntExp 0)
                  ],
                X.Return (X.name "x")
              ]
          ]
    let expected =
          parseProgram
            [ "let rec solve : int =",
              "    let $2: $1 one_tuple = if true",
              "        then let x: $3 = 1 in (x,)",
              "        else let x: $5 = 0 in (x,)",
              "    in let x: $1 = $2.0",
              "    in x",
              "in",
              "solve"
            ]
    (Y.formatProgram <$> run' prog) `shouldBe` Right (Y.formatProgram expected)
