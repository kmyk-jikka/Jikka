module Jikka.Language.Python.Typed.Type where

import Jikka.Language.Name
import Jikka.Language.Python.Typed.Stdlib

type Type = CurryType Expr

data Expr
  = Var VarName
  | Lit Literal
  | UnOp UnaryOp Expr
  | BinOp BinaryOp Expr Expr
  | TerOp TernaryOp Expr Expr
  | Sub VarName [Expr]
  | ListComp Type Expr (Maybe VarName) Expr (Maybe Expr)
  | ListExt Type [Expr]
  | Call FunName [Expr]
  deriving (Eq, Ord, Show, Read)

data Sentence
  = If Expr [Sentence] [Sentence]
  | For VarName Expr [Sentence]
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
