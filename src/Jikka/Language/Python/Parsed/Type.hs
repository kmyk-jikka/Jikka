module Jikka.Language.Python.Parsed.Type where

import Jikka.Language.Common.Name
import Jikka.Language.Common.Pos

type Type' = WithPos Type

type Expr' = WithPos Expr

type Sentence' = WithPos Sentence

type ToplevelDecl' = WithPos ToplevelDecl

data Type
  = TyInt
  | TyNat
  | TyInterval Expr' Expr'
  | TyBool
  | TyList Type'
  | TyIterator Type'
  | TyArray Type' Expr'
  deriving (Eq, Ord, Show, Read)

data Literal
  = LitInt Integer
  | LitBool Bool
  deriving (Eq, Ord, Show, Read)

data Comprehension = Comprehension Expr' (Maybe VarName) Expr' (Maybe Expr')
  deriving (Eq, Ord, Show, Read)

data Expr
  = Lit Literal
  | Var VarName
  | Sub Expr' Expr'
  | ListExt [Expr']
  | ListComp Comprehension
  | IterComp Comprehension
  | Call FunName [Expr']
  | Cond Expr' Expr' Expr'
  deriving (Eq, Ord, Show, Read)

data ListShape
  = NoneShape
  | ListShape ListShape Expr'
  deriving (Eq, Ord, Show, Read)

data Sentence
  = If Expr' [Sentence'] [Sentence']
  | For VarName Expr' [Sentence']
  | Define VarName (Maybe Type') Expr'
  | Declare VarName (Maybe Type') ListShape
  | Assign VarName [Expr'] Expr'
  | Assert Expr'
  | Return Expr'
  deriving (Eq, Ord, Show, Read)

data ToplevelDecl
  = ConstDef VarName (Maybe Type') Expr'
  | FunDef FunName [(VarName, Maybe Type')] (Maybe Type') [Sentence']
  | FromImport [Name]
  deriving (Eq, Ord, Show, Read)

newtype Program
  = Program
      { decls :: [ToplevelDecl']
      }
  deriving (Eq, Ord, Show, Read)
