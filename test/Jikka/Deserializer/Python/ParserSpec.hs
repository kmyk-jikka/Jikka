module Jikka.Deserializer.Python.ParserSpec
  ( spec,
  )
where

import Data.Char (toLower)
import Jikka.Deserializer.Pos
import qualified Jikka.Deserializer.Python.Lexer as L
import Jikka.Deserializer.Python.Parser
import Jikka.Language.Python.Parsed.Type
import Test.Hspec

run' :: [L.Token] -> Either String Program
run' tokens = run $ zipWith (\x token -> WithPos (Pos 1 x) token) [1 ..] tokens

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          concat
            [ [L.Def, L.Ident "solve", L.OpenParen, L.CloseParen, L.Arrow, L.Ident "int", L.Colon, L.Newline],
              [L.Indent, L.Return, L.Int 42, L.Newline],
              [L.Dedent]
            ]
    let parsed = Program [FunDef (FunName "solve") [] TyInt [Return (Lit (LitInt 42))]]
    run' input `shouldBe` Right parsed
  it "works on a small fun def" $ do
    let input =
          concat
            [ [L.Def, L.Ident "solve", L.OpenParen, L.Ident "p", L.Colon, L.Ident "bool", L.CloseParen, L.Arrow, L.Ident "int", L.Colon, L.Newline],
              [L.Indent, L.If, L.Ident "p", L.Colon, L.Newline],
              [L.Indent, L.Return, L.Int 0, L.Newline],
              [L.Dedent, L.Else, L.Colon, L.Newline],
              [L.Indent, L.Return, L.Int 1, L.Newline],
              [L.Dedent],
              [L.Dedent]
            ]
    let parsed =
          Program
            [FunDef (FunName "solve") [(VarName "p", TyBool)] TyInt [If (Var (VarName "p")) [Return (Lit (LitInt 0))] [Return (Lit (LitInt 1))]]]
    run' input `shouldBe` Right parsed
  it "works on a simple constant def" $ do
    let input = [L.Ident "MOD", L.Colon, L.Ident "int", L.Equal, L.Int 1000000007, L.Newline]
    let parsed =
          Program
            [ConstDef (VarName "MOD") TyInt (Lit (LitInt 1000000007))]
    run' input `shouldBe` Right parsed
