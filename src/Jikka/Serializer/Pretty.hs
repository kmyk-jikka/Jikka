-- TODO: replace this with Jikka.Serializer.ML
module Jikka.Serializer.Pretty
  ( run,
  )
where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Text (Text, pack)
import Jikka.Language.Type
import Text.Show

type Prec = Int

f :: String -> ShowS
f = showString

type' :: Prec -> Type -> ShowS
type' d t = case t of
  TyVar x -> f x
  TyUnit -> f "unit"
  TyBool -> f "bool"
  TyInt -> f "int"
  TyFun t1 t2 -> let prec = 5 in showParen (d > prec) $ type' (prec + 1) t1 . f " -> " . type' prec t2

literal :: Literal -> String
literal lit = case lit of
  Unit -> "()"
  Int n -> show n
  Bool p -> map toLower $ show p

builtIn :: BuiltIn -> String
builtIn a = case a of
  And -> "&&"
  Or -> "||"
  Neg -> "negate"
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Mod -> "%"
  Pow -> "**"
  Equal -> "=="
  NotEqual -> "/="
  LessThan -> "<"
  LessThanEqual -> "<="
  GreaterThan -> ">"
  GreaterThanEqual -> ">="
  _ -> case show a of
    [] -> []
    c : s -> toLower c : s

funType :: FunType -> String
funType NoRec = "function"
funType (Rec x) = "fixpoint " ++ x

branch :: ([Pattern], Expr) -> String
branch (pats, e) = "| " ++ unwords (map pattern' pats) ++ " -> " ++ expr 0 e "" ++ "\n"

pattern' :: Pattern -> String
pattern' pat = case pat of
  PatVar x -> x
  PatLit lit -> literal lit
  PatPlusK x k -> "(" ++ x ++ " + " ++ show k ++ ")"

expr :: Prec -> Expr -> ShowS
expr d e = case e of
  Lit lit -> f $ literal lit
  Var x -> f x
  BuiltIn a -> f $ builtIn a
  Let x t e1 e2 -> let prec = 5 in showParen (d > prec) $ f ("let " ++ x ++ " : ") . type' 0 t . f " = " . expr d e1 . f " in\n" . expr d e2
  Fun ftype branches -> let prec = 5 in showParen (d > prec) . f $ funType ftype ++ "\n" ++ concatMap branch branches ++ "end"
  App e1 e2 -> let prec = 10 in showParen (d > prec) $ expr prec e1 . f " " . expr (prec + 1) e2

given' :: (Name, Type) -> ShowS
given' (x, t) = f ("let given " ++ x ++ " : ") . type' 0 t . f " in\n"

run :: Program -> Either String Text
run prog = Right . pack $ prog' ""
  where
    concatFun = foldl (.) id
    prog' = concatFun (map given' (given prog)) . expr 0 (body prog)
