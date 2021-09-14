-- |
-- Module      : Jikka.CPlusPlus.Language.Expr
-- Description : contains data types of C++ language. / C++ のためのデータ型を含みます。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.CPlusPlus.Language.Expr` module has the basic data types for C++ language.
-- The data types are intended to use for the code generation.
module Jikka.CPlusPlus.Language.Expr where

import Data.String
import Jikka.Common.Name

data NameHint
  = LocalNameHint
  | LocalArgumentNameHint
  | LoopCounterNameHint
  | ConstantNameHint
  | FunctionNameHint
  | ArgumentNameHint
  | AdHocNameHint String
  deriving (Eq, Ord, Show, Read)

data VarName = VarName OccName NameFlavour (Maybe NameHint) deriving (Eq, Ord, Show, Read)

instance IsString VarName where
  fromString s =
    let (occ, flavour) = toFlavouredName s
     in VarName occ flavour Nothing

formatVarName :: VarName -> String
formatVarName (VarName occ flavour _) = formatFlavouredName occ flavour

newtype FunName = FunName String deriving (Eq, Ord, Show, Read)

instance IsString FunName where
  fromString = FunName

formatFunName :: FunName -> String
formatFunName (FunName occ) = occ

data Type
  = -- | @auto@
    TyAuto
  | -- | @void@
    TyVoid
  | -- | @bool@
    TyBool
  | -- | @int@
    TyInt
  | -- | @int32_t@
    TyInt32
  | -- | @int64_t@
    TyInt64
  | -- | @std::tuple\<T1, T2, ...\>@
    TyTuple [Type]
  | -- | @std::vector\<T\>@
    TyVector Type
  | -- | @std::arrya\<T, n\>@
    TyArray Type Integer
  | -- | @std::string@
    TyString
  | -- | @std::function\<Tr (T1, T2, ...)\>@
    TyFunction Type [Type]
  | -- | @jikka::convex_hull_trick@
    TyConvexHullTrick
  | -- | @atcoder::segtree\<T, op, e\>@
    TySegmentTree Monoid'
  | -- | an integer @n@ for template parameters
    TyIntValue Integer
  deriving (Eq, Ord, Show, Read)

data Monoid'
  = -- | \((\mathbb{Z}, +, 0)\)
    MonoidIntPlus
  | -- | \((\mathrm{int64\_t}, \min, \mathrm{INT64\_MAX})\)
    MonoidIntMin
  | -- | \((\mathrm{int64\_t}, \max, \mathrm{INT64\_MIN})\)
    MonoidIntMax
  | -- | \((\mathbb{Z}, \gcd, 0)\)
    MonoidIntGcd
  | -- | \((\mathbb{Z}, \mathrm{lcm}, 1)\)
    MonoidIntLcm
  deriving (Eq, Ord, Show, Read)

data Literal
  = LitInt32 Integer
  | LitInt64 Integer
  | LitBool Bool
  | LitChar Char
  | LitString String
  deriving (Eq, Ord, Show, Read)

data Function
  = -- | other functions
    Function FunName [Type]
  | -- | other methods
    Method FunName
  | -- | subscription @e1[e2]@
    At
  | -- | updated array @auto tmp = e1; tmp[e2] = e3; return tmp;@
    SetAt Type
  | -- | cast @(T)e@
    Cast Type
  | -- | functio @std::tuple\<T1, T2, ...\>(e1, e2, ...)@
    StdTuple [Type]
  | -- | function @std::get\<T, n\>(e)@
    StdGet Integer
  | -- | @std::array\<T, n\>{e1, e2, ..., en}@
    ArrayExt Type
  | -- | @std::vector\<T\>{e1, e2, ...}@
    VecExt Type
  | -- | constructors @std::vector\<T\>()@ / @std::vector\<T\>(n)@ / @std::vector\<T\>(n, e)@
    VecCtor Type
  | -- | function @std::vector\<int\> jikka::range(int n)@, which is similar to Python's @range@ or Boost's @boost::range@
    Range
  | -- | @size@ method of @std::vector\<T\>@
    MethodSize
  | -- | the constructor of @jikka::convex_hull_trick@
    ConvexHullTrickCtor
  | -- | This makes a copy of @jikka::convex_hull_trick@ and updates it. This is removed at `Jikka.CPlusPlus.Convert.MoveSemantics.run`.
    ConvexHullTrickCopyAddLine
  | -- | the constructors of @atcoder::segtree\<T, op, e\>@
    SegmentTreeCtor Monoid'
  | -- | This makes a copy of @atcoder::segtree\<T, op, e\>@ and updates it. This is removed at `Jikka.CPlusPlus.Convert.MoveSemantics.run`.
    SegmentTreeCopySetPoint Monoid'
  deriving (Eq, Ord, Show, Read)

data UnaryOp
  = -- | @+@
    IntNop
  | -- | @-@
    Negate
  | -- | @~@
    BitNot
  | -- | @!@ / @not@
    Not
  | -- | @*@
    Deref
  deriving (Eq, Ord, Show, Read)

data BinaryOp
  = -- | @+@
    Add
  | -- | @-@
    Sub
  | -- | @*@
    Mul
  | -- | @/@
    Div
  | -- | @%@
    Mod
  | -- | @&@
    BitAnd
  | -- | @|@
    BitOr
  | -- | @^@
    BitXor
  | -- | @\<\<@
    BitLeftShift
  | -- | @\>\>@
    BitRightShift
  | -- | @&&@ / @and@
    And
  | -- | @||@ / @or@
    Or
  | -- | @\<@
    LessThan
  | -- | @\<=@
    LessEqual
  | -- | @\>@
    GreaterThan
  | -- | @\>=@
    GreaterEqual
  | -- | @==@
    Equal
  | -- | @!=@
    NotEqual
  deriving (Eq, Ord, Show, Read)

data AssignOp
  = -- | @=@
    SimpleAssign
  | -- | @+=@
    AddAssign
  | -- | @-=@
    SubAssign
  | -- | @*=@
    MulAssign
  | -- | @/=@
    DivAssign
  | -- | @%=@
    ModAssign
  | -- | @\<\<=@
    BitLeftShiftAssign
  | -- | @\>\>=@
    BitRightShiftAssign
  | -- | @&=@
    BitAndAssign
  | -- | @|=@
    BitOrAssign
  | -- | @^=@
    BitXorAssign
  deriving (Eq, Ord, Show, Read)

data Expr
  = Var VarName
  | Lit Literal
  | UnOp UnaryOp Expr
  | BinOp BinaryOp Expr Expr
  | -- | @e1 ? e2 : e3@
    Cond Expr Expr Expr
  | -- | lambda expression @[=](T1 x1, T2 x2, ...) -> Tr { stmt1; stmt2; ... }@
    Lam [(Type, VarName)] Type [Statement]
  | -- | @f(e1, e2, ...)@ for a callable @f@
    Call Expr [Expr]
  | Callable Function
  deriving (Eq, Ord, Show, Read)

data LeftExpr
  = -- | @x@
    LeftVar VarName
  | -- | @e[i]@
    LeftAt LeftExpr Expr
  | -- | @std::get\<n\>@
    LeftGet Integer LeftExpr
  deriving (Eq, Ord, Show, Read)

data AssignExpr
  = -- | @e1 = e2@
    AssignExpr AssignOp LeftExpr Expr
  | -- | @++ e@
    AssignIncr LeftExpr
  | -- | @-- e@
    AssignDecr LeftExpr
  deriving (Eq, Ord, Show, Read)

data DeclareRight
  = -- | @T x;@
    DeclareDefault
  | -- | @T x = e;@
    DeclareCopy Expr
  | -- | @T x(e1, e2, ...);@. This is only for better formatting. This should not be used while optimization phases.
    DeclareInitialize [Expr]
  deriving (Eq, Ord, Show, Read)

data Statement
  = -- | @e;@
    ExprStatement Expr
  | -- | @{ stmt1; stmts2; ...; }@
    Block [Statement]
  | -- | @if (e) { stmt1; stmts2; ...; }@ / @if (e) { stmt1; stmts2; ...; } else { stmt1'; stmt2'; ...; }@
    If Expr [Statement] (Maybe [Statement])
  | -- | @for (T x = e1; e2; e3) { stmt1; stmts2; ...; }@
    For Type VarName Expr Expr AssignExpr [Statement]
  | -- | @for (T x : e) { stmt1; stmts2; ...; }@
    ForEach Type VarName Expr [Statement]
  | -- | @while (e) { stmt1; stmts2; ...; }@
    While Expr [Statement]
  | -- | Declarations with/witout initializations. See `DeclareRight`.
    Declare Type VarName DeclareRight
  | -- | @auto [x1, x2, ...] = e;@
    DeclareDestructure [VarName] Expr
  | -- | @e1 op= e2;@
    Assign AssignExpr
  | -- | @assert (e);@
    Assert Expr
  | -- | @return e;@
    Return Expr
  deriving (Eq, Ord, Show, Read)

data ToplevelStatement
  = -- | @const T x = e;@
    VarDef Type VarName Expr
  | -- | @T f(T1 x1, T2 x2, ...) { stmt1; stmt2; ... }@
    FunDef Type FunName [(Type, VarName)] [Statement]
  | -- | @static_assert(e, msg);@
    StaticAssert Expr String
  deriving (Eq, Ord, Show, Read)

newtype Program = Program
  { decls :: [ToplevelStatement]
  }
  deriving (Eq, Ord, Show, Read)
