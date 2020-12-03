module Jikka.RestrictedPython.Language.Expr where

import Jikka.Common.Language.Name
import Jikka.RestrictedPython.Language.Stdlib

type Type = CurryType Expr

data Comprehension = Comprehension Type Expr VarName Type Expr (Maybe Expr)
  deriving (Eq, Ord, Show, Read)

data Expr
  = Var VarName
  | Lit Literal
  | UnOp UnaryOp Expr
  | BinOp BinaryOp Expr Expr
  | TerOp TernaryOp Expr Expr Expr
  | Sub Type Expr Expr
  | ListExt Type [Expr]
  | ListComp Comprehension
  | IterComp Comprehension
  | Call FunName [Expr]
  deriving (Eq, Ord, Show, Read)

data Sentence
  = If Expr [Sentence] [Sentence]
  | For VarName Type Expr [Sentence]
  | Declare VarName Type [Expr]
  | Assign VarName [Expr] Expr
  | Define VarName Type Expr
  | Assert Expr
  | Return Expr
  deriving (Eq, Ord, Show, Read)

data ToplevelDecl
  = ConstDef VarName Type Expr
  | FunDef FunName [(VarName, Type)] Type [Sentence]
  deriving (Eq, Ord, Show, Read)

newtype Program
  = Program
      { decls :: [ToplevelDecl]
      }
  deriving (Eq, Ord, Show, Read)