{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.CPlusPlus.Format
-- Description : converts the AST of C++ to strings. / C++ の抽象構文木を文字列に変換します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.CPlusPlus.Format` module converts the AST for C++ to the plain source code.
module Jikka.CPlusPlus.Format
  ( run,
    run',
    Code,
    formatExpr,
    formatType,
  )
where

import Data.List (intercalate)
import Data.Text (Text, pack)
import Jikka.CPlusPlus.Language.Expr
import Jikka.CPlusPlus.Language.Util
import Jikka.Common.Format.AutoIndent (makeIndentFromBraces)

type Code = String

-- | <https://docs.microsoft.com/en-us/cpp/cpp/cpp-built-in-operators-precedence-and-associativity>
data Prec
  = IdentPrec
  | ScopeResolutionPrec
  | -- | the precidense of function calls
    FunCallPrec
  | -- | the precidense of @!@ and @~@
    UnaryPrec
  | PointerToMemberPrec
  | -- | the precidense of @*@, @/@, and @%@
    MultPrec
  | -- | the precidense of @+@, @-@
    AddPrec
  | -- | the precidense of @<<@, @>>@
    ShiftPrec
  | -- | the precidense of @<@, @<=@, @>@, and @>=@
    LessThanPrec
  | -- | the precidense of @==@ and @!=@
    EqualPrec
  | -- | the precidense of @&@
    BitAndPrec
  | -- | the precidense of @^@
    BitXorPrec
  | -- | the precidense of @|@
    BitOrPrec
  | -- | the precidense of @&&@
    AndPrec
  | -- | the precidense of @||@
    OrPrec
  | -- | the precidense of the conditional operator @?@ and @:@
    CondPrec
  | -- | the precidense of the conditional operator @=@, @+=@, @-=@, ...
    AssignPrec
  | ThrowPrec
  | -- | the precidense of @,@
    CommaPrec
  | ParenPrec
  deriving (Eq, Ord, Show, Read)

data Assoc
  = NoAssoc
  | LeftToRight
  | RightToLeft
  deriving (Eq, Ord, Show, Read)

assocOf :: Prec -> Assoc
assocOf = \case
  IdentPrec -> NoAssoc
  ScopeResolutionPrec -> NoAssoc
  FunCallPrec -> LeftToRight
  UnaryPrec -> RightToLeft
  PointerToMemberPrec -> LeftToRight
  MultPrec -> LeftToRight
  AddPrec -> LeftToRight
  ShiftPrec -> LeftToRight
  LessThanPrec -> LeftToRight
  EqualPrec -> LeftToRight
  BitAndPrec -> LeftToRight
  BitXorPrec -> LeftToRight
  BitOrPrec -> LeftToRight
  AndPrec -> LeftToRight
  OrPrec -> LeftToRight
  CondPrec -> RightToLeft
  AssignPrec -> RightToLeft
  ThrowPrec -> RightToLeft
  CommaPrec -> LeftToRight
  ParenPrec -> NoAssoc

formatUnaryOp :: UnaryOp -> (Code, Prec)
formatUnaryOp = \case
  IntNop -> ("+", UnaryPrec)
  Negate -> ("-", UnaryPrec)
  BitNot -> ("~", UnaryPrec)
  Not -> ("not", UnaryPrec)

formatBinaryOp :: BinaryOp -> (Code, Prec)
formatBinaryOp = \case
  Add -> ("+", AddPrec)
  Sub -> ("-", AddPrec)
  Mul -> ("*", MultPrec)
  Div -> ("/", MultPrec)
  Mod -> ("%", MultPrec)
  BitLeftShift -> ("<<", ShiftPrec)
  BitRightShift -> (">>", ShiftPrec)
  LessThan -> ("<", LessThanPrec)
  LessEqual -> ("<=", LessThanPrec)
  GreaterThan -> (">", LessThanPrec)
  GreaterEqual -> (">=", LessThanPrec)
  Equal -> ("==", EqualPrec)
  NotEqual -> ("!=", EqualPrec)
  BitAnd -> ("&", BitAndPrec)
  BitXor -> ("^", BitXorPrec)
  BitOr -> ("|", BitOrPrec)
  And -> ("and", AndPrec)
  Or -> ("or", OrPrec)

formatAssignOp :: AssignOp -> (Code, Prec)
formatAssignOp = \case
  SimpleAssign -> ("=", AssignPrec)
  AddAssign -> ("+=", AssignPrec)
  SubAssign -> ("-=", AssignPrec)
  MulAssign -> ("*=", AssignPrec)
  DivAssign -> ("/=", AssignPrec)
  ModAssign -> ("%=", AssignPrec)
  BitLeftShiftAssign -> ("<<=", AssignPrec)
  BitRightShiftAssign -> (">>=", AssignPrec)
  BitAndAssign -> ("&=", AssignPrec)
  BitOrAssign -> ("|=", AssignPrec)
  BitXorAssign -> ("^=", AssignPrec)

-- | `resolvePrec` inserts parens to the given string if required.
--
-- >>> resolvePrec MultPrec ("1 + 2", AddPrec) ++ " * 3"
-- "(1 + 2) * 3"
--
-- >>> resolvePrec AddPrec ("1 * 2", MultPrec) ++ " + 3"
-- "1 * 2 + 3"
--
-- >>> resolvePrec CommaPrec ("1, 2", CommaPrec) ++ ", 3"
-- "1, 2, 3"
resolvePrec :: Prec -> (Code, Prec) -> Code
resolvePrec cur (s, prv)
  | cur < prv = "(" ++ s ++ ")"
  | otherwise = s

-- | `resolvePrecLeft` inserts parens to the given string if required.
--
-- >>> resolvePrecLeft AddPrec ("1 - 2", AddPrec) ++ " - 3"
-- "1 - 2 - 3"
resolvePrecLeft :: Prec -> (Code, Prec) -> Code
resolvePrecLeft cur (s, prv)
  | cur < prv || (cur == prv && assocOf cur /= LeftToRight) = "(" ++ s ++ ")"
  | otherwise = s

-- | `resolvePrecRight` inserts parens to the given string if required.
--
-- >>> "1 - " ++ resolvePrecRight AddPrec ("2 - 3", AddPrec)
-- "1 - (2 - 3)"
resolvePrecRight :: Prec -> (Code, Prec) -> Code
resolvePrecRight cur (s, prv)
  | cur < prv || (cur == prv && assocOf cur /= RightToLeft) = "(" ++ s ++ ")"
  | otherwise = s

formatType :: Type -> Code
formatType = \case
  TyAuto -> "auto"
  TyVoid -> "void"
  TyInt -> "int"
  TyInt32 -> "int32_t"
  TyInt64 -> "int64_t"
  TyBool -> "bool"
  TyTuple ts -> "std::tuple<" ++ intercalate ", " (map formatType ts) ++ ">"
  TyVector t -> "std::vector<" ++ formatType t ++ ">"
  TyArray t n -> "std::array<" ++ formatType t ++ ", " ++ show n ++ ">"
  TyString -> "std::string"
  TyFunction t ts -> "std::function<" ++ formatType t ++ " (" ++ intercalate ", " (map formatType ts) ++ ")>"
  TyIntValue n -> show n

formatLiteral :: Literal -> Code
formatLiteral = \case
  LitInt32 n -> show n
  LitInt64 n -> show n
  LitBool p -> if p then "true" else "false"
  LitChar c -> show c
  LitString s -> show s

formatExpr' :: Prec -> Expr -> Code
formatExpr' prec = resolvePrec prec . formatExpr

formatExpr :: Expr -> (Code, Prec)
formatExpr = \case
  Var x -> (unVarName x, IdentPrec)
  Lit lit -> (formatLiteral lit, IdentPrec)
  UnOp op e ->
    let (op', prec) = formatUnaryOp op
        e' = formatExpr' prec e
     in (op' ++ " " ++ e', prec)
  BinOp op e1 e2 ->
    let (op', prec) = formatBinaryOp op
        e1' = resolvePrecLeft prec (formatExpr e1)
        e2' = resolvePrecRight prec (formatExpr e2)
     in (e1' ++ " " ++ op' ++ " " ++ e2', prec)
  Lam args ret body ->
    let args' = map (\(t, x) -> formatType t ++ " " ++ unVarName x) args
        ret' = formatType ret
        body' = concatMap formatStatement body
     in ("[=](" ++ intercalate ", " args' ++ ") -> " ++ ret' ++ "{ " ++ unwords body' ++ " }", FunCallPrec)
  Call f args -> case f of
    Callable f -> (formatExpr' FunCallPrec f ++ "(" ++ intercalate ", " (map (formatExpr' CommaPrec) args) ++ ")", FunCallPrec)
    Function f ts ->
      let template = if null ts then "" else "<" ++ intercalate ", " (map formatType ts) ++ ">"
       in (unFunName f ++ template ++ "(" ++ intercalate ", " (map (formatExpr' CommaPrec) args) ++ ")", FunCallPrec)
    Method x f -> (formatExpr' FunCallPrec x ++ "." ++ unFunName f ++ "(" ++ intercalate ", " (map (formatExpr' CommaPrec) args) ++ ")", FunCallPrec)
  Cond e1 e2 e3 ->
    let e1' = resolvePrecLeft CondPrec (formatExpr e1)
        e2' = resolvePrec CondPrec (formatExpr e2)
        e3' = resolvePrecRight CondPrec (formatExpr e3)
     in (e1' ++ " ? " ++ e2' ++ " : " ++ e3', CondPrec)
  VecExt t es ->
    let es' = concatMap (formatExpr' CommaPrec) es
     in ("std::vector<" ++ formatType t ++ ">{" ++ es' ++ "}", IdentPrec)
  At e1 e2 ->
    let e1' = formatExpr' FunCallPrec e1
        e2' = formatExpr' FunCallPrec e2
     in (e1' ++ "[" ++ e2' ++ "]", FunCallPrec)
  Cast t e ->
    let t' = formatType t
        e' = formatExpr' ParenPrec e
     in ("static_cast<" ++ t' ++ ">(" ++ e' ++ ")", FunCallPrec)

formatLeftExpr :: LeftExpr -> (Code, Prec)
formatLeftExpr = formatExpr . fromLeftExpr

formatAssignExpr :: AssignExpr -> (Code, Prec)
formatAssignExpr = \case
  AssignExpr op e1 e2 ->
    let (op', prec) = formatAssignOp op
        e1' = resolvePrecLeft prec (formatLeftExpr e1)
        e2' = resolvePrecRight prec (formatExpr e2)
     in (e1' ++ " " ++ op' ++ " " ++ e2', AssignPrec)
  AssignIncr e -> ("++ " ++ resolvePrec UnaryPrec (formatLeftExpr e), UnaryPrec)
  AssignDecr e -> ("-- " ++ resolvePrec UnaryPrec (formatLeftExpr e), UnaryPrec)

formatStatement :: Statement -> [Code]
formatStatement = \case
  ExprStatement e -> [formatExpr' ParenPrec e ++ ";"]
  Block stmts -> ["{"] ++ concatMap formatStatement stmts ++ ["}"]
  If e body1 body2 ->
    let e' = formatExpr' ParenPrec e
        body1' = concatMap formatStatement body1
     in case body2 of
          Nothing -> ["if (" ++ e' ++ ") {"] ++ body1' ++ ["}"]
          Just body2 -> case concatMap formatStatement body2 of
            (('i' : 'f' : ' ' : '(' : line) : lines) -> ["if (" ++ e' ++ ") {"] ++ body1' ++ ["} else if (" ++ line] ++ lines
            body2 -> ["if (" ++ e' ++ ") {"] ++ body1' ++ ["} else {"] ++ body2 ++ ["}"]
  For t x init cond incr body ->
    let t' = formatType t
        init' = formatExpr' ParenPrec init
        cond' = formatExpr' ParenPrec cond
        incr' = resolvePrec ParenPrec $ formatAssignExpr incr
        body' = concatMap formatStatement body
     in ["for (" ++ t' ++ " " ++ unVarName x ++ " = " ++ init' ++ "; " ++ cond' ++ "; " ++ incr' ++ ") {"] ++ body' ++ ["}"]
  ForEach t x xs body ->
    let t' = formatType t
        xs' = formatExpr' ParenPrec xs
        body' = concatMap formatStatement body
     in ["for (" ++ t' ++ " " ++ unVarName x ++ " : " ++ xs' ++ ") {"] ++ body' ++ ["}"]
  While cond body ->
    let cond' = formatExpr' ParenPrec cond
        body' = concatMap formatStatement body
     in ["while (" ++ cond' ++ ") {"] ++ body' ++ ["}"]
  Declare t x e ->
    let t' = formatType t
        e' = case e of
          Nothing -> ""
          Just e -> " = " ++ resolvePrecRight AssignPrec (formatExpr e)
     in [t' ++ " " ++ unVarName x ++ e' ++ ";"]
  DeclareDestructure xs e -> ["auto [" ++ intercalate ", " (map unVarName xs) ++ "] = " ++ resolvePrecRight AssignPrec (formatExpr e) ++ ";"]
  Assign e -> [resolvePrec ParenPrec (formatAssignExpr e) ++ ";"]
  Assert e -> ["assert (" ++ formatExpr' ParenPrec e ++ ");"]
  Return e -> ["return " ++ formatExpr' ParenPrec e ++ ";"]

formatToplevelStatement :: ToplevelStatement -> [Code]
formatToplevelStatement = \case
  VarDef t x e -> [formatType t ++ " " ++ unVarName x ++ " = " ++ resolvePrecRight AssignPrec (formatExpr e) ++ ";"]
  FunDef ret f args body ->
    let ret' = formatType ret
        args' = intercalate ", " $ map (\(t, x) -> formatType t ++ " " ++ unVarName x) args
        body' = concatMap formatStatement body
     in [ret' ++ " " ++ unVarName f ++ "(" ++ args' ++ ") {"] ++ body' ++ ["}"]

formatProgram :: Program -> [Code]
formatProgram prog = headers ++ concatMap formatToplevelStatement (decls prog)
  where
    headers =
      [ "#include <algorithm>",
        "#include <cstdint>",
        "#include <functional>",
        "#include <iostream>",
        "#include <numeric>",
        "#include <string>",
        "#include <tuple>",
        "#include <vector>",
        "#include \"jikka/all.hpp\""
      ]

run' :: Program -> String
run' = unlines . makeIndentFromBraces 4 . formatProgram

run :: Applicative m => Program -> m Text
run = pure . pack . run'
