module Jikka.Language.Core.Type
  ( Alt (..),
    AltCon (..),
    Bind (..),
    DataCon (..),
    Expr (..),
    Program (..),
    TyCon (..),
    Type (..),
    TyVar (..),
    Var (..),
    -- re-export
    BuiltIn (..),
    Literal (..),
    Name (..),
  )
where

import Jikka.Language.Common.BuiltIn (BuiltIn (..))
import Jikka.Language.Common.Literal (Literal (..))
import Jikka.Language.Common.Name (Name (..))

data Var
  = MkVar
      { varName :: Name,
        varType :: Type
      }
  deriving (Eq, Ord, Show, Read)

type TyVar = Var

data TyCon
  = UnitTyCon
  | BoolTyCon
  | IntTyCon
  | ListTyCon
  deriving (Eq, Ord, Show, Read)

-- | 'Type' represents types of the second-order lambda calculus, System F
data Type
  = TyVarTy TyVar
  | TyConApp TyCon [Type]
  | AppTy Type Type
  | FunTy Type Type
  | ForAllTy TyVar Type
  deriving (Eq, Ord, Show, Read)

data Bind
  = NonRec Var Expr
  | Rec [(Var, Expr)]
  deriving (Eq, Ord, Show, Read)

type Alt = (AltCon, [Var], Expr)

type DataCon = Name

data AltCon
  = DataAlt DataCon
  | LitAlt Literal
  | DEFAULT
  deriving (Eq, Ord, Show, Read)

data Expr
  = Var Var
  | Lit Literal
  | App Expr Expr
  | Lam Var Expr
  | Let Bind Expr
  | Case Expr Var Type [Alt]
  deriving (Eq, Ord, Show, Read)

data TopLevelBind
  = TopLevelGiven Var
  | TopLevelNonRec Var Expr
  | TopLevelRec [(Var, Expr)]
  deriving (Eq, Ord, Show, Read)

type Program = [TopLevelBind]
