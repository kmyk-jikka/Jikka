module Jikka.Python.Parse.HappySpec
  ( spec,
  )
where

import Jikka.Common.Error (Error)
import Jikka.Common.Language.Name
import Jikka.Common.Location
import Jikka.Python.Language.Expr
import Jikka.Python.Parse.Happy
import qualified Jikka.Python.Parse.Token as L
import Test.Hspec

at :: a -> (Int, Int) -> WithLoc a
at token (y, x) = WithLoc (Loc y x (-1)) token

run' :: [[L.Token]] -> Either Error Program
run' tokens = run . concat $ zipWith (\y -> zipWith (\x token -> token `at` (y, x)) [1 ..]) [1 ..] tokens

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          [ [L.Def, L.Ident "solve", L.OpenParen, L.CloseParen, L.Arrow, L.Ident "int", L.Colon, L.Newline],
            [L.Indent, L.Return, L.Int 42, L.Newline],
            [L.Dedent]
          ]
    let parsed = Program [FunDef (FunName "solve") [] (Just $ TyInt `at` (1, 6)) [Return (Lit (LitInt 42) `at` (2, 3)) `at` (2, 2)] `at` (1, 1)]
    run' input `shouldBe` Right parsed
  it "works on a small fun def" $ do
    let input =
          [ [L.Def, L.Ident "solve", L.OpenParen, L.Ident "p", L.CloseParen, L.Colon, L.Newline],
            [L.Indent, L.If, L.Ident "p", L.Colon, L.Newline],
            [L.Indent, L.Return, L.Int 0, L.Newline],
            [L.Dedent, L.Else, L.Colon, L.Newline],
            [L.Indent, L.Return, L.Int 1, L.Newline],
            [L.Dedent],
            [L.Dedent]
          ]
    let parsed =
          Program
            [FunDef (FunName "solve") [(VarName "p", Nothing)] Nothing [If (Var (VarName "p") `at` (2, 3)) [Return (Lit (LitInt 0) `at` (3, 3)) `at` (3, 2)] [Return (Lit (LitInt 1) `at` (5, 3)) `at` (5, 2)] `at` (2, 2)] `at` (1, 1)]
    run' input `shouldBe` Right parsed
  it "works on a simple constant def" $ do
    let input = [[L.Ident "MOD", L.Colon, L.Ident "int", L.Equal, L.Int 1000000007, L.Newline]]
    let parsed =
          Program
            [ConstDef (VarName "MOD") (Just $ TyInt `at` (1, 3)) (Lit (LitInt 1000000007) `at` (1, 5)) `at` (1, 1)]
    run' input `shouldBe` Right parsed
