module Jikka.Language.Python.Type where

type Name = String

newtype VarName = VarName {unVarName :: Name} deriving (Eq, Ord, Show, Read)

newtype FunName = FunName {unFunName :: Name} deriving (Eq, Ord, Show, Read)

data Type
  = TyInt
  | TyNat
  | TyInterval Integer Integer
  | TyBool
  | TyList Type
  | TyIterator Type
  | TyArray Type Integer
  deriving (Eq, Ord, Show, Read)

data Literal
  = LitInt Integer
  | LitBool Bool
  deriving (Eq, Ord, Show, Read)

data Expr
  = Lit Literal
  | ListComp Expr (Maybe VarName) Expr (Maybe Expr)
  | ListExt [Expr]
  | Var VarName
  | Sub Expr Expr
  | Call FunName [Expr]
  | Cond Expr Expr Expr
  deriving (Eq, Ord, Show, Read)

data Sentence
  = If Expr [Sentence] [Sentence]
  | For VarName Expr [Sentence]
  | Declare VarName Type
  | Assign VarName Expr
  | Define VarName Type Expr
  | Assert Expr
  | Return Expr
  deriving (Eq, Ord, Show, Read)

data ToplevelDecl
  = ConstDef VarName Type Expr
  | FunDef FunName [Type] Type [Sentence]
  deriving (Eq, Ord, Show, Read)

data Program
  = Program
      { isCompatImported :: Bool,
        decls :: [ToplevelDecl]
      }
  deriving (Eq, Ord, Show, Read)
