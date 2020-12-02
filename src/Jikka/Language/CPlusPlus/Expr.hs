{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Jikka.Language.CPlusPlus.Expr
-- Description : contains data types of C++ language.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.CPlusPlus.Expr` module has the basic data types for C++ language.
-- The data types are intended to use for the code generation.
module Jikka.Language.CPlusPlus.Expr where

import Data.String (IsString)

newtype VarName = VarName {unVarName :: String} deriving (Eq, Ord, Show, Read, IsString)

newtype FunName = FunName {unFunName :: String} deriving (Eq, Ord, Show, Read, IsString)

data Type
  = TyAuto
  | TyVoid
  | TyBool
  | TyInt32
  | TyInt64
  | TyTuple [Type]
  | TyVector Type
  | TyArray Type Integer
  deriving (Eq, Ord, Show, Read)

data Literal
  = LitInt32 Integer
  | LitInt64 Integer
  | LitBool Bool
  deriving (Eq, Ord, Show, Read)

data Function
  = Function FunName
  | Method FunName
  deriving (Eq, Ord, Show, Read)

data UnaryOp
  = IntNop
  | Negate
  | BitNot
  | Not
  deriving (Eq, Ord, Show, Read)

data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | BitAnd
  | BitOr
  | BitXor
  | LeftShift
  | RightShift
  | And
  | Or
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | Equal
  | NotEqual
  deriving (Eq, Ord, Show, Read)

data AssignOp
  = SimpleAssign
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | LeftShiftAssign
  | RightShiftAssign
  | BitAndAssign
  | BitOrAssign
  | BitXorAssign
  deriving (Eq, Ord, Show, Read)

data Expr
  = Var VarName
  | Lit Literal
  | UnOp UnaryOp Expr
  | BinOp BinaryOp Expr Expr
  | Cond Expr Expr Expr
  | Call FunName [Expr]
  | VecExt Type [Expr]
  | At Expr Expr
  | Cast Type Expr
  deriving (Eq, Ord, Show, Read)

data LeftExpr
  = LeftVar VarName
  | LeftAt LeftExpr Expr
  deriving (Eq, Ord, Show, Read)

data AssignExpr
  = AssignExpr AssignOp LeftExpr Expr
  | AssignIncr LeftExpr
  | AssignDecr LeftExpr
  deriving (Eq, Ord, Show, Read)

data Statement
  = ExprStatement Expr
  | Block [Statement]
  | If Expr [Statement] (Maybe [Statement])
  | For Type VarName Expr Expr AssignExpr [Statement]
  | ForEach Type VarName Expr [Statement]
  | While Expr [Statement]
  | Declare Type VarName (Maybe Expr)
  | Assign AssignExpr
  | Assert Expr
  | Return Expr
  deriving (Eq, Ord, Show, Read)

data ToplevelStatement
  = VarDef Type VarName Expr
  | FunDef Type FunName [(Type, VarName)] [Statement]
  deriving (Eq, Ord, Show, Read)

newtype Program
  = Program
      { decls :: [ToplevelStatement]
      }
  deriving (Eq, Ord, Show, Read)
