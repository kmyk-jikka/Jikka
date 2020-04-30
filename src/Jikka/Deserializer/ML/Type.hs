{-# LANGUAGE DeriveGeneric #-}

module Jikka.Deserializer.ML.Type where

import Control.DeepSeq
import GHC.Generics (Generic)
import Jikka.Deserializer.ML.Pos

type Name = String

data Type
  = TyVar Name
  | TyFun Type Type
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData Type

data Literal
  = Unit
  | Int Integer
  | Bool Bool
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData Literal

type Args = [(Maybe Name, Maybe (WithPos Type))]

data MatchPattern
  = PatVar (Maybe Name)
  | PatLit Literal
  | PatPlusK (Maybe Name) Integer
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData MatchPattern

type MatchBranch = ([MatchPattern], WithPos Expr)

data LetType
  = NoRec
  | Rec
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData LetType

data Expr
  = Lit Literal
  | Var Name
  | App (WithPos Expr) (WithPos Expr)
  | Let LetType (Maybe Name) Args (Maybe (WithPos Type)) (WithPos Expr) (WithPos Expr)
  | Fun Args (WithPos Expr)
  | If (WithPos Expr) (WithPos Expr) (WithPos Expr)
  | Match (WithPos Expr) [MatchBranch]
  | Function [MatchBranch]
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData Expr

data Program
  = Program
      { given :: [(Name, WithPos Type)],
        body :: WithPos Expr
      }
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData Program
