module Jikka.Language.Python.Typed.Stdlib where

import Jikka.Language.Common.Name

-- Church-style types
data ChurchType
  = TyInt
  | TyBool
  | TyList ChurchType
  | TyIterator ChurchType
  | TyVar TypeName
  deriving (Eq, Ord, Show, Read)

-- Curry-style types
data CurryType expr
  = ATyInt
  | ATyBool
  | ATyList (CurryType expr)
  | ATyNat
  | ATyInterval expr expr
  | ATyIterator (CurryType expr)
  | ATyArray (CurryType expr) expr
  | ATyVar TypeName
  deriving (Eq, Ord, Show, Read)

toChurchType :: CurryType expr -> ChurchType
toChurchType t = case t of
  ATyInt -> TyInt
  ATyBool -> TyBool
  ATyList t' -> TyList (toChurchType t')
  ATyNat -> TyInt
  ATyInterval _ _ -> TyInt
  ATyIterator t' -> TyIterator (toChurchType t')
  ATyArray t' _ -> TyList (toChurchType t')
  ATyVar name -> TyVar name

toCurryType :: ChurchType -> CurryType expr
toCurryType t = case t of
  TyInt -> ATyInt
  TyBool -> ATyBool
  TyList t' -> ATyList (toCurryType t')
  TyIterator t' -> ATyIterator (toCurryType t')
  TyVar name -> ATyVar name

-- 0-ary functions
data Literal
  = LitInt Integer
  | LitBool Bool
  deriving (Eq, Ord, Show, Read)

-- 1-ary functions
data UnaryOp
  = -- arithmetical functions
    Negate
  | Fact
  | Abs
  | -- logical functions
    Not
  | -- bitwise functions
    BitNot
  | -- list functions
    Len ChurchType
  | Sum
  | Product
  | Min1
  | Max1
  | ArgMin
  | ArgMax
  | All
  | Any
  | Sorted ChurchType
  | List ChurchType
  | Reversed ChurchType
  | Range1
  deriving (Eq, Ord, Show, Read)

-- 2-ary functions (including 2-ary relations)
data BinaryOp
  = -- arithmetical functions
    Plus
  | Minus
  | Mult
  | FloorDiv
  | FloorMod
  | CeilDiv
  | CeilMod
  | Pow
  | Gcd
  | Lcm
  | Min
  | Max
  | -- modular functions
    Inv
  | -- combinational functions
    Choose
  | Permute
  | MultiChoose
  | -- logical functions
    And
  | Or
  | Implies
  | -- bitwise functions
    BitAnd
  | BitOr
  | BitXor
  | BitLeftShift
  | BitRightShift
  | -- list functions
    Range2
  | -- arithmetical relations
    LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | -- equality relations (polymorphic)
    Equal ChurchType
  | NotEqual ChurchType
  deriving (Eq, Ord, Show, Read)

-- 3-ary functions
data TernaryOp
  = -- conditional functions (polymorphic)
    Cond ChurchType
  | -- modular functions
    PowMod
  | -- list functions
    Range3
  deriving (Eq, Ord, Show, Read)
