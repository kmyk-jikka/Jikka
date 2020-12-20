{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Jikka.RestrictedPython.Language.Expr
-- Description : contains data types of the restricted Python.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Language.Expr
  ( Ident (..),
    unIdent,
    Type (..),
    Constant (..),
    Target (..),
    Comprehension (..),
    Expr (..),
    Statement (..),
    ToplevelStatement (..),
    Program,
    BoolOp (..),
    CmpOp (..),
    Operator (..),
    UnaryOp (..),
  )
where

import Data.String (IsString)
import Jikka.Python.Language.Expr (BoolOp (..), CmpOp (..), Operator (..), UnaryOp (..))

newtype Ident = Ident String deriving (Eq, Ord, Show, Read, IsString)

unIdent :: Ident -> String
unIdent (Ident x) = x

data Type
  = VarTy Ident
  | NoneTy
  | IntTy
  | BoolTy
  | ListTy Type
  | IteratorTy Type
  | SequenceTy Type
  | TupleTy [Type]
  | CallableTy [Type] Type
  deriving (Eq, Ord, Show, Read)

data Constant
  = ConstNone
  | ConstInt Integer
  | ConstBool Bool
  deriving (Eq, Ord, Show, Read)

data Target
  = SubscriptTrg Type Ident [Expr]
  | NameTrg Ident
  | TupleTrg [(Ident, Type)]
  deriving (Eq, Ord, Show, Read)

data Comprehension = Comprehension Target Type Expr (Maybe Expr)
  deriving (Eq, Ord, Show, Read)

data Expr
  = BoolOp Expr BoolOp Expr
  | BinOp Expr Operator Expr
  | UnaryOp UnaryOp Expr
  | Lambda [(Ident, Type)] Type Expr
  | IfExp Type Expr Expr Expr
  | ListComp Type Expr Comprehension
  | Compare Expr CmpOp Expr
  | Call Type Expr [Expr]
  | Constant Constant
  | Subscript Type Expr Expr
  | Name Ident
  | List Type [Expr]
  | Tuple [(Expr, Type)]
  | SubscriptSlice Type Expr (Maybe Expr) (Maybe Expr) (Maybe Expr)
  deriving (Eq, Ord, Show, Read)

data Statement
  = Return Expr
  | AugAssign Target Operator Expr
  | AnnAssign Target Type Expr
  | For Target Type Expr [Statement]
  | If Expr [Statement] [Statement]
  | Assert Expr
  deriving (Eq, Ord, Show, Read)

data ToplevelStatement
  = ToplevelAnnAssign Ident Type Expr
  | ToplevelFunctionDef Ident [(Ident, Type)] [Statement] Type
  | ToplevelAssert Expr
  deriving (Eq, Ord, Show, Read)

type Program = [ToplevelStatement]
