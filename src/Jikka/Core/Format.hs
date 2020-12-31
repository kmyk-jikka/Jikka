{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Format
-- Description : converts the expr of core language to strings.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- TODO: add parens with considering precedences.
module Jikka.Core.Format
  ( run,
    run',
  )
where

import Data.List (intercalate)
import Data.Text (Text, pack)
import Jikka.Common.Format.AutoIndent
import Jikka.Core.Language.Expr

paren :: String -> String
paren s = "(" ++ s ++ ")"

formatType :: Type -> String
formatType = \case
  VarTy (TypeName a) -> a
  IntTy -> "int"
  BoolTy -> "bool"
  ListTy t -> formatType t ++ " list"
  TupleTy ts -> paren $ intercalate " * " (map formatType ts)
  FunTy ts ret -> paren $ intercalate " * " (map formatType ts) ++ " -> " ++ formatType ret

data Builtin'
  = Fun [Type] String
  | PrefixOp String
  | InfixOp [Type] String
  | At' Type
  | If' Type
  deriving (Eq, Ord, Show, Read)

fun :: String -> Builtin'
fun = Fun []

infixOp :: String -> Builtin'
infixOp = InfixOp []

analyzeBuiltin :: Builtin -> Builtin'
analyzeBuiltin = \case
  -- arithmetical functions
  Negate -> PrefixOp "negate"
  Plus -> infixOp "+"
  Minus -> infixOp "-"
  Mult -> infixOp "*"
  FloorDiv -> infixOp "/"
  FloorMod -> infixOp "%"
  CeilDiv -> fun "ceildiv"
  CeilMod -> fun "ceilmod"
  Pow -> infixOp "**"
  -- induction functions
  NatInd t -> Fun [t] "ind"
  -- advanced arithmetical functions
  Abs -> fun "abs"
  Gcd -> fun "gcd"
  Lcm -> fun "lcm"
  Min -> fun "min"
  Max -> fun "max"
  -- logical functions
  Not -> PrefixOp "not"
  And -> infixOp "and"
  Or -> infixOp "or"
  Implies -> infixOp "implies"
  If t -> If' t
  -- bitwise functions
  BitNot -> PrefixOp "~"
  BitAnd -> infixOp "&"
  BitOr -> infixOp "|"
  BitXor -> infixOp "^"
  BitLeftShift -> infixOp "<<"
  BitRightShift -> infixOp ">>"
  -- modular functions
  Inv -> fun "inv"
  PowMod -> fun "powmod"
  -- list functions
  Len t -> Fun [t] "len"
  Tabulate t -> Fun [t] "tabulate"
  Map t1 t2 -> Fun [t1, t2] "map"
  At t -> At' t
  Sum -> fun "sum"
  Product -> fun "product"
  Min1 -> fun "min1"
  Max1 -> fun "max1"
  ArgMin -> fun "argmin"
  ArgMax -> fun "argmax"
  All -> fun "all"
  Any -> fun "any"
  Sorted t -> Fun [t] "sort"
  List t -> Fun [t] "list"
  Reversed t -> Fun [t] "reverse"
  Range1 -> fun "range1"
  Range2 -> fun "range2"
  Range3 -> fun "range3"
  -- arithmetical relations
  LessThan -> infixOp "<"
  LessEqual -> infixOp "<="
  GreaterThan -> infixOp ">"
  GreaterEqual -> infixOp ">="
  -- equality relations (polymorphic)
  Equal t -> InfixOp [t] "=="
  NotEqual t -> InfixOp [t] "!="
  -- combinational functions
  Fact -> fun "fact"
  Choose -> fun "choose"
  Permute -> fun "permute"
  MultiChoose -> fun "multichoose"

formatTemplate :: [Type] -> String
formatTemplate = \case
  [] -> ""
  ts -> "<" ++ intercalate ", " (map formatType ts) ++ ">"

formatFunCall :: String -> [Type] -> [Expr] -> String
formatFunCall f _ args = f ++ "(" ++ intercalate ", " (map formatExpr args) ++ ")"

formatBuiltinIsolated :: Builtin' -> String
formatBuiltinIsolated = \case
  Fun ts name -> name ++ formatTemplate ts
  PrefixOp op -> paren op
  InfixOp ts op -> paren $ op ++ formatTemplate ts
  At' t -> paren $ "at" ++ formatTemplate [t]
  If' t -> paren $ "if-then-else" ++ formatTemplate [t]

formatBuiltin :: Builtin' -> [Expr] -> String
formatBuiltin builtin args = case (builtin, args) of
  (Fun ts name, _) -> formatFunCall name ts args
  (PrefixOp op, [e1]) -> paren $ op ++ " " ++ formatExpr e1
  (InfixOp _ op, [e1, e2]) -> paren $ formatExpr e1 ++ " " ++ op ++ " " ++ formatExpr e2
  (At' _, [e1, e2]) -> paren $ formatExpr e1 ++ ")[" ++ formatExpr e2 ++ "]"
  (If' _, [e1, e2, e3]) -> paren $ "if" ++ " " ++ formatExpr e1 ++ " then " ++ formatExpr e2 ++ " else " ++ formatExpr e3
  _ -> formatFunCall (formatBuiltinIsolated builtin) [] args

formatLiteral :: Literal -> String
formatLiteral = \case
  LitBuiltin builtin -> formatBuiltinIsolated (analyzeBuiltin builtin)
  LitInt n -> show n
  LitBool p -> show p

formatFormalArgs :: [(VarName, Type)] -> String
formatFormalArgs args = unwords $ map (\(x, t) -> paren (unVarName x ++ ": " ++ formatType t)) args

formatExpr :: Expr -> String
formatExpr = \case
  Var x -> unVarName x
  Lit lit -> formatLiteral lit
  App f args -> case f of
    Var x -> formatFunCall (unVarName x) [] args
    Lit (LitBuiltin builtin) -> formatBuiltin (analyzeBuiltin builtin) args
    _ -> formatFunCall (formatExpr f) [] args
  Lam args e -> paren $ "fun " ++ formatFormalArgs args ++ " ->\n" ++ indent ++ "\n" ++ formatExpr e ++ "\n" ++ dedent ++ "\n"
  Let x t e1 e2 -> "let " ++ unVarName x ++ ": " ++ formatType t ++ " =\n" ++ indent ++ "\n" ++ formatExpr e1 ++ "\n" ++ dedent ++ "\nin " ++ formatExpr e2

formatRecKind :: RecKind -> String
formatRecKind = \case
  NonRec -> "let "
  Rec -> "let rec "

formatToplevelExpr :: ToplevelExpr -> [String]
formatToplevelExpr = \case
  ResultExpr e -> [formatExpr e]
  ToplevelLet rec f args ret e cont ->
    [ formatRecKind rec ++ unVarName f ++ " " ++ formatFormalArgs args ++ ": " ++ formatType ret ++ " =",
      indent
    ]
      ++ lines (formatExpr e)
      ++ [dedent]
      ++ ["in"]
      ++ formatToplevelExpr cont

formatProgram :: Program -> [String]
formatProgram = formatToplevelExpr

run' :: Program -> String
run' = unlines . makeIndentFromMarkers 4 . formatProgram

run :: Applicative m => Program -> m Text
run = pure . pack . run'
