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
  | Abs
  | -- combinational functions
    Fact
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

literalType :: Literal -> CurryType expr
literalType lit = case lit of
  LitInt n -> if n >= 0 then ATyNat else ATyInt
  LitBool _ -> ATyBool

unaryOpType :: UnaryOp -> (CurryType expr, CurryType expr)
unaryOpType op =
  let ii = (ATyInt, ATyInt)
      nn = (ATyNat, ATyNat)
      bb = (ATyBool, ATyBool)
   in case op of
        -- arithmetical functions
        Negate -> ii
        Fact -> nn
        Abs -> (ATyInt, ATyNat)
        -- logical functions
        Not -> bb
        -- bitwise functions
        BitNot -> ii
        -- list functions
        Len t -> let t' = toCurryType t in (ATyList t', ATyNat)
        Sum -> (ATyIterator ATyInt, ATyInt)
        Product -> (ATyIterator ATyInt, ATyInt)
        Min1 -> (ATyIterator ATyInt, ATyInt)
        Max1 -> (ATyIterator ATyInt, ATyInt)
        ArgMin -> (ATyIterator ATyInt, ATyNat)
        ArgMax -> (ATyIterator ATyInt, ATyNat)
        All -> (ATyIterator ATyBool, ATyBool)
        Any -> (ATyIterator ATyBool, ATyBool)
        Sorted t -> let t' = toCurryType t in (ATyList t', ATyList t')
        List t -> let t' = toCurryType t in (ATyIterator t', ATyList t')
        Reversed t -> let t' = toCurryType t in (ATyIterator t', ATyList t')
        Range1 -> (ATyInt, ATyIterator ATyNat)

binaryOpType :: BinaryOp -> (CurryType expr, CurryType expr, CurryType expr)
binaryOpType op =
  let iii = (ATyInt, ATyInt, ATyInt)
      bbb = (ATyInt, ATyInt, ATyInt)
      inn = (ATyInt, ATyNat, ATyNat)
      iib = (ATyInt, ATyInt, ATyBool)
      nnn = (ATyNat, ATyNat, ATyNat)
   in case op of
        -- arithmetical functions
        Plus -> iii
        Minus -> iii
        Mult -> iii
        FloorDiv -> iii
        FloorMod -> iii
        CeilDiv -> iii
        CeilMod -> iii
        Pow -> iii
        Gcd -> iii
        Lcm -> iii
        Min -> iii
        Max -> iii
        -- modular functions
        Inv -> inn
        -- combinational functions
        Choose -> nnn
        Permute -> nnn
        MultiChoose -> nnn
        -- logical functions
        And -> bbb
        Or -> bbb
        Implies -> bbb
        -- bitwise functions
        BitAnd -> iii
        BitOr -> iii
        BitXor -> iii
        BitLeftShift -> iii
        BitRightShift -> iii
        -- list functions
        Range2 -> (ATyInt, ATyInt, ATyIterator ATyInt)
        -- arithmetical relations
        LessThan -> iib
        LessEqual -> iib
        GreaterThan -> iib
        GreaterEqual -> iib
        -- equality relations (polymorphic)
        Equal t -> let t' = toCurryType t in (t', t', ATyBool)
        NotEqual t -> let t' = toCurryType t in (t', t', ATyBool)

ternaryOpType :: TernaryOp -> (CurryType expr, CurryType expr, CurryType expr, CurryType expr)
ternaryOpType op = case op of
  Cond t -> let t' = toCurryType t in (ATyBool, t', t', t')
  PowMod -> (ATyInt, ATyInt, ATyNat, ATyNat)
  Range3 -> (ATyInt, ATyInt, ATyInt, ATyIterator ATyInt)
