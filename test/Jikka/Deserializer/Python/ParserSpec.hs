module Jikka.Deserializer.Python.ParserSpec
  ( spec,
  )
where

import qualified Jikka.Deserializer.Python.Lexer as L
import Jikka.Deserializer.Python.Parser
import Jikka.Language.Common.Name
import Jikka.Language.Common.Pos
import Jikka.Language.Python.Parsed.Expr
import Test.Hspec

at :: a -> (Int, Int) -> WithPos a
at token (y, x) = WithPos (Pos y x) token

run' :: [[L.Token]] -> Either String Program
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
