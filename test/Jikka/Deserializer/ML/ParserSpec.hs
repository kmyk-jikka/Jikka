{-# LANGUAGE OverloadedStrings #-}

module Jikka.Deserializer.ML.ParserSpec
  ( spec,
  )
where

import Data.Char (toLower)
import qualified Jikka.Deserializer.ML.Lexer as L
import Jikka.Deserializer.ML.Parser
import Jikka.Deserializer.ML.Pos
import Jikka.Deserializer.ML.Type
import Test.Hspec

prettyLiteral :: Literal -> String
prettyLiteral lit = case lit of
  Unit -> "()"
  Int n -> show n
  Bool p -> map toLower $ show p

prettyOptName :: Maybe Name -> String
prettyOptName Nothing = "_"
prettyOptName (Just x) = x

prettyType :: Type -> String
prettyType (TyVar a) = a
prettyType (TyFun t1 t2) = "(" ++ prettyType t1 ++ " -> " ++ prettyType t2 ++ ")"

prettyOptType :: Maybe (WithPos Type) -> String
prettyOptType Nothing = ""
prettyOptType (Just t) = ": " ++ prettyType (value t) ++ " "

prettyArgs :: Args -> String
prettyArgs [] = ""
prettyArgs ((x, Nothing) : args) = prettyOptName x ++ " " ++ prettyArgs args
prettyArgs ((x, Just t) : args) = "(" ++ prettyOptName x ++ " : " ++ prettyType (value t) ++ ")" ++ prettyArgs args

prettyMatch :: MatchPattern -> String
prettyMatch (PatVar x) = " " ++ prettyOptName x
prettyMatch (PatLit lit) = " " ++ prettyLiteral lit
prettyMatch (PatPlusK x k) = "(" ++ prettyOptName x ++ " + " ++ show k ++ ") "

prettyBranch :: MatchBranch -> String
prettyBranch (patterns, e) = "| " ++ concatMap prettyMatch patterns ++ "-> " ++ prettyExpr e ++ "\n"

prettyExpr :: WithPos Expr -> String
prettyExpr e = case value e of
  Lit lit -> prettyLiteral lit
  Var x -> x
  App WithPos {value = App e1 e2} e3 -> "(" ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ " " ++ prettyExpr e3 ++ ")"
  App e1 e2 -> "(" ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
  Let x args t e1 e2 -> "(let " ++ prettyOptName x ++ " " ++ prettyArgs args ++ prettyOptType t ++ "= " ++ prettyExpr e1 ++ " in " ++ prettyExpr e2 ++ ")"
  LetRec x args t e1 e2 -> "(let rec " ++ x ++ " " ++ prettyArgs args ++ prettyOptType t ++ "= " ++ prettyExpr e1 ++ " in " ++ prettyExpr e2 ++ ")"
  Fun args e -> "(fun " ++ prettyArgs args ++ "-> " ++ prettyExpr e ++ ")"
  If e1 e2 e3 -> "(if " ++ prettyExpr e1 ++ "\nthen " ++ prettyExpr e2 ++ "\nelse " ++ prettyExpr e3 ++ ")"
  Match e branches -> "(match " ++ prettyExpr e ++ " with" ++ concatMap prettyBranch branches ++ "end)"
  Function branches -> "(function" ++ concatMap prettyBranch branches ++ "end)"

run' :: String -> Either String String
run' input = do
  tokens <- L.run input
  parsed <- run tokens
  return $ prettyExpr parsed

spec :: Spec
spec = describe "parser" $ do
  it "arith simple" $ do
    let input = "a + b"
    let pretty = "(+ a b)"
    run' input `shouldBe` Right pretty
  it "let simple" $ do
    let input = "let x = 0 in x"
    let pretty = "(let x = 0 in x)"
    run' input `shouldBe` Right pretty
  it "let nested" $ do
    let input = "let x = let y = 0 in 1 in let z = 2 in 3"
    let pretty = "(let x = (let y = 0 in 1) in (let z = 2 in 3))"
    run' input `shouldBe` Right pretty
  it "shunting-yard algorithm" $ do
    let input = "(x + 1) ** 2 == x ** 2 + 2 * x + 1"
    let pretty = "(== (** (+ x 1) 2) (+ (+ (** x 2) (* 2 x)) 1))"
    run' input `shouldBe` Right pretty
