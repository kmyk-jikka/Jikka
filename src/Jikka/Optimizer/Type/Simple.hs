module Jikka.Optimizer.Type.Simple where

data Literal
  = Unit
  | Bool Bool
  | Int Integer
  deriving (Eq, Ord, Read, Show)

type Name = String

data FunType
  = NoRec
  | Rec Name
  deriving (Eq, Ord, Read, Show)

data Expr
  = Lit Literal
  | Var Name
  | Let Name Expr Expr
  | App Expr Expr
  | Fun FunType [([Pattern], Expr)]
  deriving (Eq, Ord, Read, Show)

data Pattern
  = PatAny
  | PatLit Literal
  | PatVar Name
  | PatPlusK Name Integer
  deriving (Eq, Ord, Read, Show)
