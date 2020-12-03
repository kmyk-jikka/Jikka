module Jikka.Language.Parsed.Type
  ( Expr (..),
    FunType (..),
    Pattern (..),
    Program (..),
    Type (..),
    -- re-export
    BuiltIn (..),
    Literal (..),
    Name (..),
  )
where

import Jikka.Language.Common.BuiltIn (BuiltIn (..))
import Jikka.Language.Common.Literal (Literal (..))
import Jikka.Language.Common.Name (Name (..))

data Type
  = TyVar Name
  | TyUnit
  | TyBool
  | TyInt
  | TyFun Type Type
  deriving (Eq, Ord, Read, Show)

data FunType
  = NoRec
  | Rec !Name
  deriving (Eq, Ord, Read, Show)

data Pattern
  = PatVar Name
  | PatLit !Literal
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

data Program
  = Program
      { given :: [(Name, Type)],
        body :: Expr
      }
  deriving (Eq, Ord, Read, Show)
