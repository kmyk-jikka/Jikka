module Jikka.Language.Python.Parsed.Type where

type Name = String

newtype VarName = VarName {unVarName :: Name} deriving (Eq, Ord, Show, Read)

newtype FunName = FunName {unFunName :: Name} deriving (Eq, Ord, Show, Read)

data Type
  = TyInt
  | TyNat
  | TyInterval Expr Expr
  | TyBool
  | TyList Type
  | TyIterator Type
  | TyArray Type Expr
  deriving (Eq, Ord, Show, Read)

data Literal
  = LitInt Integer
  | LitBool Bool
  deriving (Eq, Ord, Show, Read)

data Expr
  = Lit Literal
  | Var VarName
  | Sub Expr Expr
  | ListComp Expr (Maybe VarName) Expr (Maybe Expr)
  | ListExt [Expr]
  | Call FunName [Expr]
  | Cond Expr Expr Expr
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
  | FromImport [String]
  deriving (Eq, Ord, Show, Read)

newtype Program
  = Program
      { decls :: [ToplevelDecl]
      }
  deriving (Eq, Ord, Show, Read)
