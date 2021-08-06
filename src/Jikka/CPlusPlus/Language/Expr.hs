{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Jikka.CPlusPlus.Language.Expr
-- Description : contains data types of C++ language. / C++ のためのデータ型を含みます。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.CPlusPlus.Language.Expr` module has the basic data types for C++ language.
-- The data types are intended to use for the code generation.
module Jikka.CPlusPlus.Language.Expr where

import Data.String (IsString)

newtype VarName = VarName {unVarName :: String} deriving (Eq, Ord, Show, Read, IsString)

newtype FunName = FunName {unFunName :: String} deriving (Eq, Ord, Show, Read, IsString)

data Type
  = TyAuto
  | TyVoid
  | TyBool
  | TyInt
  | TyInt32
  | TyInt64
  | TyTuple [Type]
  | TyVector Type
  | TyArray Type Integer
  | TyString
  | TyFunction Type [Type]
  | TyConvexHullTrick
  | TySegmentTree Monoid'
  | -- | for template parameters
    TyIntValue Integer
  deriving (Eq, Ord, Show, Read)

data Monoid'
  = MonoidIntPlus
  | MonoidIntMin
  | MonoidIntMax
  | MonoidIntGcd
  | MonoidIntLcm
  deriving (Eq, Ord, Show, Read)

data Literal
  = LitInt32 Integer
  | LitInt64 Integer
  | LitBool Bool
  | LitChar Char
  | LitString String
  deriving (Eq, Ord, Show, Read)

data Function
  = Function FunName [Type]
  | Method FunName
  | At
  | Cast Type
  | StdTuple [Type]
  | StdGet Integer
  | ArrayExt Type
  | VecExt Type
  | VecCtor Type
  | Range
  | MethodSize
  | ConvexHullTrickCtor
  | ConvexHullTrickCopyAddLine
  | SegmentTreeCtor Monoid'
  | SegmentTreeCopySetPoint Monoid'
  deriving (Eq, Ord, Show, Read)

data UnaryOp
  = IntNop
  | Negate
  | BitNot
  | Not
  | Deref
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
  | BitLeftShift
  | BitRightShift
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
  | BitLeftShiftAssign
  | BitRightShiftAssign
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
  | Lam [(Type, VarName)] Type [Statement]
  | Call Function [Expr]
  | CallExpr Expr [Expr]
  deriving (Eq, Ord, Show, Read)

data LeftExpr
  = LeftVar VarName
  | LeftAt LeftExpr Expr
  | -- | @std::get<n>@
    LeftGet Integer LeftExpr
  deriving (Eq, Ord, Show, Read)

data AssignExpr
  = AssignExpr AssignOp LeftExpr Expr
  | AssignIncr LeftExpr
  | AssignDecr LeftExpr
  deriving (Eq, Ord, Show, Read)

data DeclareRight
  = DeclareDefault
  | DeclareCopy Expr
  | -- | This is only for better formatting. This should not be used while optimization phases.
    DeclareInitialize [Expr]
  deriving (Eq, Ord, Show, Read)

data Statement
  = ExprStatement Expr
  | Block [Statement]
  | If Expr [Statement] (Maybe [Statement])
  | For Type VarName Expr Expr AssignExpr [Statement]
  | ForEach Type VarName Expr [Statement]
  | While Expr [Statement]
  | Declare Type VarName DeclareRight
  | DeclareDestructure [VarName] Expr
  | Assign AssignExpr
  | Assert Expr
  | Return Expr
  deriving (Eq, Ord, Show, Read)

data ToplevelStatement
  = VarDef Type VarName Expr
  | FunDef Type VarName [(Type, VarName)] [Statement]
  deriving (Eq, Ord, Show, Read)

newtype Program = Program
  { decls :: [ToplevelStatement]
  }
  deriving (Eq, Ord, Show, Read)
