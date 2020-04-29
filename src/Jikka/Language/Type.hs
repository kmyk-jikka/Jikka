module Jikka.Language.Type where

type Name = String

data Type
  = TyVar Name
  | TyUnit
  | TyBool
  | TyInt
  | TyFun Type Type
  deriving (Eq, Ord, Read, Show)

data Literal
  = Unit
  | Bool !Bool
  | Int !Integer
  deriving (Eq, Ord, Read, Show)

data BuiltIn
  = Not
  | And
  | Or
  | If
  | Succ
  | Pred
  | Neg
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Min
  | Max
  | Sum
  | Product
  | Minimum
  | Maximum
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

data FunType
  = NoRec
  | Rec !Name
  deriving (Eq, Ord, Read, Show)

data Pattern
  = PatAny
  | PatLit !Literal
  | PatVar Name
  | PatPlusK Name Integer
  deriving (Eq, Ord, Read, Show)

data Expr
  = Lit Literal
  | Var Name
  | BuiltIn BuiltIn
  | Let Name Type Expr Expr
  | Fun FunType [([Pattern], Expr)]
  | App Expr Expr
  deriving (Eq, Ord, Read, Show)
