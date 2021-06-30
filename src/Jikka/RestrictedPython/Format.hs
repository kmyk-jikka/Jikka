{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.RestrictedPython.Format
-- Description : converts the AST of the restricted Python to strings.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- TODO: add parens with considering precedences.
module Jikka.RestrictedPython.Format
  ( run,
    run',
    formatType,
    formatOperator,
    formatBuiltin,
  )
where

import Data.List (intercalate)
import Data.Text (Text, pack)
import Jikka.Common.Format.AutoIndent
import Jikka.RestrictedPython.Language.Builtin
import Jikka.RestrictedPython.Language.Expr

formatType :: Type -> String
formatType t = case t of
  VarTy x -> unTypeName x
  IntTy -> "int"
  BoolTy -> "bool"
  ListTy t -> "List[" ++ formatType t ++ "]"
  TupleTy [] -> "None"
  TupleTy ts -> "Tuple[" ++ intercalate ", " (map formatType ts) ++ "]"
  CallableTy ts ret -> "Callable[[" ++ intercalate ", " (map formatType ts) ++ "], " ++ formatType ret ++ "]"

formatConstant :: Constant -> String
formatConstant = \case
  ConstNone -> "None"
  ConstInt n -> show n
  ConstBool p -> show p
  ConstBuiltin b -> formatBuiltin b

formatBoolOp :: BoolOp -> String
formatBoolOp = \case
  And -> "and"
  Or -> "or"
  Implies -> "implies"

formatOperator :: Operator -> String
formatOperator = \case
  Add -> "+"
  Sub -> "-"
  Mult -> "*"
  MatMult -> "@"
  Div -> "/"
  FloorDiv -> "//"
  FloorMod -> "%"
  CeilDiv -> "/^"
  CeilMod -> "%^"
  Pow -> "**"
  BitLShift -> "<<"
  BitRShift -> ">>"
  BitOr -> "|"
  BitXor -> "^"
  BitAnd -> "&"
  Max -> ">?"
  Min -> "<?"

formatUnaryOp :: UnaryOp -> String
formatUnaryOp = \case
  Invert -> "~"
  Not -> "not"
  UAdd -> "+"
  USub -> "-"

formatCmpOp :: CmpOp' -> String
formatCmpOp (CmpOp' op _) = case op of
  Eq' -> "=="
  NotEq -> "!="
  Lt -> "<"
  LtE -> "<="
  Gt -> ">"
  GtE -> ">="
  Is -> "is"
  IsNot -> "is not"
  In -> "in"
  NotIn -> "not in"

formatComprehension :: Comprehension -> String
formatComprehension (Comprehension x iter ifs) =
  let body = "for " ++ formatTarget x ++ " in " ++ formatExpr iter
      ifs' = case ifs of
        Nothing -> ""
        Just ifs -> " if " ++ formatExpr ifs
   in body ++ ifs'

formatTarget :: Target' -> String
formatTarget (WithLoc' _ x) = case x of
  SubscriptTrg x e -> formatTarget x ++ "[" ++ formatExpr e ++ "]"
  NameTrg x -> unVarName (value' x)
  TupleTrg xs -> case xs of
    [] -> "()"
    [x] -> "(" ++ formatTarget x ++ ",)"
    _ -> intercalate ", " (map formatTarget xs)

formatExpr :: Expr' -> String
formatExpr (WithLoc' _ e0) = case e0 of
  BoolOp e1 op e2 -> formatExpr e1 ++ " " ++ formatBoolOp op ++ " " ++ formatExpr e2
  BinOp e1 op e2 -> formatExpr e1 ++ " " ++ formatOperator op ++ " " ++ formatExpr e2
  UnaryOp op e -> formatUnaryOp op ++ " " ++ formatExpr e
  Lambda args body -> case args of
    [] -> "lambda: " ++ formatExpr body
    _ -> "lambda " ++ intercalate ", " (map (unVarName . value' . fst) args) ++ ": " ++ formatExpr body
  IfExp e1 e2 e3 -> formatExpr e2 ++ " if " ++ formatExpr e1 ++ " else " ++ formatExpr e3
  ListComp e comp -> "[" ++ formatExpr e ++ " " ++ formatComprehension comp ++ "]"
  Compare e1 op e2 -> formatExpr e1 ++ " " ++ formatCmpOp op ++ " " ++ formatExpr e2
  Call f args -> case args of
    [WithLoc' _ (ListComp e comp)] -> formatExpr f ++ "(" ++ formatExpr e ++ " " ++ formatComprehension comp ++ ")"
    _ -> formatExpr f ++ "(" ++ intercalate ", " (map formatExpr args) ++ ")"
  Constant const -> formatConstant const
  Subscript e1 e2 -> formatExpr e1 ++ "[" ++ formatExpr e2 ++ "]"
  Name x -> unVarName (value' x)
  List _ es -> "[" ++ intercalate ", " (map formatExpr es) ++ "]"
  Tuple es -> case es of
    [] -> "()"
    [e] -> "(" ++ formatExpr e ++ ",)"
    _ -> "(" ++ intercalate ", " (map formatExpr es) ++ ")"
  SubscriptSlice e from to step ->
    let from' = maybe "" formatExpr from
        to' = maybe "" formatExpr to
        step' = maybe "" ((':' :) . formatExpr) step
     in formatExpr e ++ "[" ++ from' ++ ":" ++ to' ++ step' ++ "]"

formatStatement :: Statement -> [String]
formatStatement = \case
  Return e -> ["return " ++ formatExpr e]
  AugAssign x op e -> [formatTarget x ++ " " ++ formatOperator op ++ "= " ++ formatExpr e]
  AnnAssign x t e -> [formatTarget x ++ ": " ++ formatType t ++ " = " ++ formatExpr e]
  For x iter body -> ["for " ++ formatTarget x ++ " in " ++ formatExpr iter ++ ":", indent] ++ concatMap formatStatement body ++ [dedent]
  If e body1 body2 -> case body2 of
    [] -> ["if " ++ formatExpr e ++ ":", indent] ++ concatMap formatStatement body1 ++ [dedent]
    [body2@(If _ _ _)] ->
      let elif : cont = formatStatement body2
       in ["if " ++ formatExpr e ++ ":", indent] ++ concatMap formatStatement body1 ++ [dedent, "el" ++ elif] ++ cont
    _ -> ["if " ++ formatExpr e ++ ":", indent] ++ concatMap formatStatement body1 ++ [dedent, "else:", indent] ++ concatMap formatStatement body2 ++ [dedent]
  Assert e -> ["assert " ++ formatExpr e]

formatToplevelStatement :: ToplevelStatement -> [String]
formatToplevelStatement = \case
  ToplevelAnnAssign x t e -> [unVarName (value' x) ++ ": " ++ formatType t ++ " = " ++ formatExpr e]
  ToplevelFunctionDef f args ret body -> ["def " ++ unVarName (value' f) ++ "(" ++ intercalate ", " (map (\(x, t) -> unVarName (value' x) ++ ": " ++ formatType t) args) ++ ") -> " ++ formatType ret ++ ":", indent] ++ concatMap formatStatement body ++ [dedent]
  ToplevelAssert e -> ["assert " ++ formatExpr e]

formatProgram :: Program -> [String]
formatProgram prog = concatMap formatToplevelStatement prog

run' :: Program -> String
run' = unlines . makeIndentFromMarkers 4 . formatProgram

run :: Applicative m => Program -> m Text
run = pure . pack . run'
