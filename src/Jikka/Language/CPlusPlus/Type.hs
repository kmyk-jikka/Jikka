module Jikka.Language.CPlusPlus.Type where

type Name = String

newtype VarName = VarName {unVarName :: Name} deriving (Eq, Ord, Show, Read)

newtype FunName = FunName {unFunName :: Name} deriving (Eq, Ord, Show, Read)

data Type
  = TyInt32
  | TyInt64
  | TyBool
  | TyVector Type
  | TyArray Type Integer
  deriving (Eq, Ord, Show, Read)

data Literal
  = LitInt32 Integer
  | LitInt64 Integer
  | LitBool Bool
  deriving (Eq, Ord, Show, Read)

data Expr
  = Lit Literal
  | VectorExt [Expr]
  | Var VarName
  | Sub Expr Expr
  | Call FunName [Expr]
  | Cond Expr Expr Expr
  deriving (Eq, Ord, Show, Read)

data Sentence
  = If Expr [Sentence] [Sentence]
  | For VarName Expr [Sentence]
  | Declare VarName Type
  | Assign VarName [Expr] Expr
  | Define VarName Type Expr
  | Assert Expr
  | Return Expr
  deriving (Eq, Ord, Show, Read)

data ToplevelDecl
  = ConstDecl VarName Type Expr
  | FunDecl Type FunName [Type] [Sentence]
  deriving (Eq, Ord, Show, Read)

newtype Program
  = Program
      { decls :: [ToplevelDecl]
      }
  deriving (Eq, Ord, Show, Read)
