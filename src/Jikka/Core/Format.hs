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
    formatBuiltinIsolated,
    formatBuiltin,
    formatType,
    formatExpr,
  )
where

import Data.Char (toLower)
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
  TupleTy ts -> case ts of
    [t] -> paren $ formatType t ++ ","
    _ -> paren $ intercalate " * " (map formatType ts)
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
  Min2 t -> Fun [t] "min"
  Max2 t -> Fun [t] "max"
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
  -- matrix functions
  MatAp _ _ -> fun "matap"
  MatZero _ -> fun "matzero"
  MatOne _ -> fun "matone"
  MatAdd _ _ -> fun "matadd"
  MatMul _ _ _ -> fun "matmul"
  MatPow _ -> fun "matpow"
  -- modular functions
  ModInv -> fun "modinv"
  ModPow -> fun "modpow"
  ModMatAp _ _ -> fun "modmatap"
  ModMatAdd _ _ -> fun "modmatadd"
  ModMatMul _ _ _ -> fun "modmatmul"
  ModMatPow _ -> fun "modmatpow"
  -- list functions
  Cons t -> Fun [t] "cons"
  Foldl t1 t2 -> Fun [t1, t2] "foldl"
  Scanl t1 t2 -> Fun [t1, t2] "scanl"
  Len t -> Fun [t] "len"
  Tabulate t -> Fun [t] "tabulate"
  Map t1 t2 -> Fun [t1, t2] "map"
  Filter t -> Fun [t] "filter"
  At t -> At' t
  SetAt t -> Fun [t] "setAt"
  Elem t -> Fun [t] "elem"
  Sum -> fun "sum"
  Product -> fun "product"
  ModProduct -> fun "modproduct"
  Min1 t -> Fun [t] "min1"
  Max1 t -> Fun [t] "max1"
  ArgMin t -> Fun [t] "argmin"
  ArgMax t -> Fun [t] "argmax"
  All -> fun "all"
  Any -> fun "any"
  Sorted t -> Fun [t] "sort"
  List t -> Fun [t] "list"
  Reversed t -> Fun [t] "reverse"
  Range1 -> fun "range1"
  Range2 -> fun "range2"
  Range3 -> fun "range3"
  -- tuple functions
  Tuple ts -> Fun ts "tuple"
  Proj ts n -> Fun ts ("proj" ++ show n)
  -- comparison
  LessThan t -> InfixOp [t] "<"
  LessEqual t -> InfixOp [t] "<="
  GreaterThan t -> InfixOp [t] ">"
  GreaterEqual t -> InfixOp [t] ">="
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
formatFunCall f _ args = f ++ "(" ++ intercalate ", " (map formatExpr' args) ++ ")"

formatBuiltinIsolated' :: Builtin' -> String
formatBuiltinIsolated' = \case
  Fun ts name -> name ++ formatTemplate ts
  PrefixOp op -> paren op
  InfixOp ts op -> paren $ op ++ formatTemplate ts
  At' t -> paren $ "at" ++ formatTemplate [t]
  If' t -> paren $ "if-then-else" ++ formatTemplate [t]

formatBuiltinIsolated :: Builtin -> String
formatBuiltinIsolated = formatBuiltinIsolated' . analyzeBuiltin

formatBuiltin' :: Builtin' -> [Expr] -> String
formatBuiltin' builtin args = case (builtin, args) of
  (Fun ts name, _) -> formatFunCall name ts args
  (PrefixOp op, [e1]) -> paren $ op ++ " " ++ formatExpr' e1
  (InfixOp _ op, [e1, e2]) -> paren $ formatExpr' e1 ++ " " ++ op ++ " " ++ formatExpr' e2
  (At' _, [e1, e2]) -> paren $ formatExpr' e1 ++ ")[" ++ formatExpr' e2 ++ "]"
  (If' _, [e1, e2, e3]) -> paren $ "if" ++ " " ++ formatExpr' e1 ++ " then " ++ formatExpr' e2 ++ " else " ++ formatExpr' e3
  _ -> formatFunCall (formatBuiltinIsolated' builtin) [] args

formatBuiltin :: Builtin -> [Expr] -> String
formatBuiltin = formatBuiltin' . analyzeBuiltin

formatLiteral :: Literal -> String
formatLiteral = \case
  LitBuiltin builtin -> formatBuiltinIsolated builtin
  LitInt n -> show n
  LitBool p -> map toLower $ show p
  LitNil t -> "nil" ++ formatTemplate [t]

formatFormalArgs :: [(VarName, Type)] -> String
formatFormalArgs args = unwords $ map (\(x, t) -> paren (unVarName x ++ ": " ++ formatType t)) args

formatExpr' :: Expr -> String
formatExpr' = \case
  Var x -> unVarName x
  Lit lit -> formatLiteral lit
  App f args -> case f of
    Var x -> formatFunCall (unVarName x) [] args
    Lit (LitBuiltin builtin) -> formatBuiltin builtin args
    _ -> formatFunCall (formatExpr' f) [] args
  Lam args e -> paren $ "fun " ++ formatFormalArgs args ++ " ->\n" ++ indent ++ "\n" ++ formatExpr' e ++ "\n" ++ dedent ++ "\n"
  Let x t e1 e2 -> "let " ++ unVarName x ++ ": " ++ formatType t ++ " =\n" ++ indent ++ "\n" ++ formatExpr' e1 ++ "\n" ++ dedent ++ "\nin " ++ formatExpr' e2

formatExpr :: Expr -> String
formatExpr = unwords . makeIndentFromMarkers 4 . lines . formatExpr'

formatToplevelExpr :: ToplevelExpr -> [String]
formatToplevelExpr = \case
  ResultExpr e -> lines (formatExpr' e)
  ToplevelLet x t e cont -> let' (unVarName x) t e cont
  ToplevelLetRec f args ret e cont -> let' ("rec " ++ unVarName f ++ " " ++ formatFormalArgs args) ret e cont
  where
    let' s t e cont =
      ["let " ++ s ++ ": " ++ formatType t ++ " =", indent]
        ++ lines (formatExpr' e)
        ++ [dedent, "in"]
        ++ formatToplevelExpr cont

formatProgram :: Program -> [String]
formatProgram = formatToplevelExpr

run' :: Program -> String
run' = unlines . makeIndentFromMarkers 4 . formatProgram

run :: Applicative m => Program -> m Text
run = pure . pack . run'
