{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Jikka.RestrictedPython.Language.Expr
-- Description : contains data types of the restricted Python. / 制限された Python のためのデータ型を含みます。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Language.Expr
  ( -- * types
    TypeName (..),
    unTypeName,
    Type (..),
    pattern NoneTy,

    -- * operators
    UnaryOp (..),
    Operator (..),
    BoolOp (..),
    CmpOp (..),
    CmpOp' (..),
    Constant (..),
    Builtin (..),
    AttributeName (..),
    unAttributeName,
    Attribute (..),
    Attribute',

    -- * exprs
    VarName (..),
    unVarName,
    module Jikka.Common.Location,
    VarName',
    Expr (..),
    Expr',
    Comprehension (..),

    -- * statements
    Target (..),
    Target',
    Statement (..),
    pattern Append,
    ToplevelStatement (..),
    Program,
  )
where

import Data.String (IsString)
import Jikka.Common.Location
import Jikka.Python.Language.Expr (BoolOp (..), CmpOp (..), Operator (..), UnaryOp (..))

newtype VarName = VarName String deriving (Eq, Ord, Show, Read, IsString)

unVarName :: VarName -> String
unVarName (VarName x) = x

type VarName' = WithLoc' VarName

newtype TypeName = TypeName String deriving (Eq, Ord, Show, Read, IsString)

unTypeName :: TypeName -> String
unTypeName (TypeName x) = x

newtype AttributeName = AttributeName String deriving (Eq, Ord, Show, Read, IsString)

unAttributeName :: AttributeName -> String
unAttributeName (AttributeName x) = x

-- | `Type` represents the types of our restricted Python-like language.
--
-- \[
--     \newcommand\int{\mathbf{int}}
--     \newcommand\bool{\mathbf{bool}}
--     \newcommand\list{\mathbf{list}}
--     \newcommand\string{\mathbf{string}}
--     \begin{array}{rl}
--         \tau ::= & \alpha \\
--         \vert & \int \\
--         \vert & \bool \\
--         \vert & \list(\tau) \\
--         \vert & \tau \times \tau \times \dots \times \tau \\
--         \vert & \tau \times \tau \times \dots \times \tau \to \tau
--         \vert & \string
--         \vert & \mathbf{side-effect}
--     \end{array}
-- \]
--
-- NOTE: \(\mathbf{None}\) is represented as the 0-tuple.
data Type
  = VarTy TypeName
  | IntTy
  | BoolTy
  | ListTy Type
  | TupleTy [Type]
  | CallableTy [Type] Type
  | StringTy
  | SideEffectTy
  deriving (Eq, Ord, Show, Read)

pattern NoneTy = TupleTy []

data Constant
  = ConstNone
  | ConstInt Integer
  | ConstBool Bool
  | ConstBuiltin Builtin
  deriving (Eq, Ord, Show, Read)

data Builtin
  = -- | "abs" \(: \int \to \int\)
    BuiltinAbs
  | -- | "pow" \((\lambda x k. x^k) : \int \times \int \to \int\)
    BuiltinPow
  | -- | modulo power "pow" \((\lambda x k m. x^k \bmod m): \int \times \int \to \int\)
    BuiltinModPow
  | -- | "divmod" \(: \int \times \int \to \int \times \int\)
    BuiltinDivMod
  | -- | ceil div \(: \int \times \int \to \int\)
    BuiltinCeilDiv
  | -- | ceil mod \(: \int \times \int \to \int\)
    BuiltinCeilMod
  | -- | floor div \(: \int \times \int \to \int\)
    BuiltinFloorDiv
  | -- | floor mod \(: \int \times \int \to \int\)
    BuiltinFloorMod
  | -- | \(\gcd: \int \times \int \to \int\)
    BuiltinGcd
  | -- | \(\mathbf{lcm}: \int \times \int \to \int\)
    BuiltinLcm
  | -- | "int" \(: \forall \alpha. \alpha \to \int\)
    BuiltinInt Type
  | -- | "bool" \(: \forall \alpha. \alpha \to \bool\)
    BuiltinBool Type
  | -- | "list" \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
    BuiltinList Type
  | -- | "tuple" \(: \forall \alpha_0 \alpha_1 \dots \alpha _ {n - 1}. \tau \to \tau\) where \(\tau = \alpha_0 \times \dots \times \alpha _ {n - 1}\)
    BuiltinTuple [Type]
  | -- | "len" \(: \forall \alpha. \list(\alpha) \to \int\)
    BuiltinLen Type
  | -- | "map" \(: \forall \alpha_0 \alpha_1 \dots \alpha_n. (\alpha_0 \times \dots \times \alpha _ {n - 1} \to \alpha_n) \times \list(\alpha_0) \times \dots \list(\alpha _ {n - 1}) \to \list(\alpha_n)\)
    BuiltinMap [Type] Type
  | -- | "sorted" \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
    BuiltinSorted Type
  | -- | "reversed" \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
    BuiltinReversed Type
  | -- | "enumerate" \(: \forall \alpha. \list(\alpha) \to \list(\int \times \alpha)\)
    BuiltinEnumerate Type
  | -- | "filter" \(: \forall \alpha. (\alpha \to \bool) \times \list(\alpha) \to \list(\alpha)\)
    BuiltinFilter Type
  | -- | "zip" \(: \forall \alpha_0 \alpha_1 \dots \alpha _ {n - 1}. \list(\alpha_0) \times \dots \list(\alpha _ {n - 1}) \to \list(\alpha_0 \times \dots \times \alpha _ {n - 1})\)
    BuiltinZip [Type]
  | -- | "all" \(: \list(\bool) \to \bool\)
    BuiltinAll
  | -- | "any" \(: \list(\bool) \to \bool\)
    BuiltinAny
  | -- | "sum" \(: \list(\int) \to \int\)
    BuiltinSum
  | -- | product \(: \list(\int) \to \int\)
    BuiltinProduct
  | -- | "range" \(: \int \to \list(\int)\)
    BuiltinRange1
  | -- | "range" \(: \int \times \int \to \list(\int)\)
    BuiltinRange2
  | -- | "range" \(: \int \times \int \times \int \to \list(\int)\)
    BuiltinRange3
  | -- | "max" \(: \forall \alpha. \list(\alpha) \to \alpha\)
    BuiltinMax1 Type
  | -- | "max" \(: \forall \alpha. \underbrace{\alpha \times \alpha \times \dots \times \alpha} _ {n ~\text{times}} \to \alpha\)
    BuiltinMax Type Int
  | -- | "min" \(: \forall \alpha. \list(\alpha) \to \alpha\)
    BuiltinMin1 Type
  | -- | "min" \(: \forall \alpha. \underbrace{\alpha \times \alpha \times \dots \times \alpha} _ {n ~\text{times}} \to \alpha\)
    BuiltinMin Type Int
  | -- | \(: \forall \alpha. \list(\alpha) \to \int\)
    BuiltinArgMax Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \int\)
    BuiltinArgMin Type
  | -- | factorial \((\lambda n. n!): \int \to \int\)
    BuiltinFact
  | -- | \((\lambda n r. {} _ n C _ r): \int \times \int \to \int\)
    BuiltinChoose
  | -- | \((\lambda n r. {} _ n P _ r): \int \times \int \to \int\)
    BuiltinPermute
  | -- | \((\lambda n r. {} _ n H _ r): \int \times \int \to \int\)
    BuiltinMultiChoose
  | -- | modulo inverse \((\lambda x m. x^{-1} \bmod m): \int \times \int \to \int\)
    BuiltinModInv
  | -- | "input" \(: \epsilon \to \string\)
    BuiltinInput
  | -- | "print" \(: \forall \alpha_0 \alpha_1 \dots \alpha _ {n - 1}. \alpha_0 \times \dots \alpha _ {n - 1} \to \epsilon\)
    BuiltinPrint [Type]
  deriving (Eq, Ord, Show, Read)

data Attribute
  = UnresolvedAttribute AttributeName
  | -- | "list.count" \(: \forall \alpha. \list(\alpha) \to \alpha \to \int\)
    BuiltinCount Type
  | -- | "list.index" \(: \forall \alpha. \list(\alpha) \to \alpha \to \int\)
    BuiltinIndex Type
  | -- | "list.copy" \(: \forall \alpha. \list(\alpha) \to \epsilon \to \list(\alpha)\)
    BuiltinCopy Type
  | -- | "list.append" \(: \forall \alpha. \list(\alpha) \to \alpha \to \mathbf{side-effect}\)
    BuiltinAppend Type
  | -- | "str.split" \(: \forall \alpha. \string \to \epsilon \to \list(\string)\)
    BuiltinSplit
  deriving (Eq, Ord, Show, Read)

type Attribute' = WithLoc' Attribute

-- | `Target` represents the lvalue of our restricted Python-like language.
--
-- \[
--     \begin{array}{rl}
--         y ::= & y \lbrack e \rbrack \\
--         \vert & x \\
--         \vert & (y, y, \dots, y) \\
--     \end{array}
-- \]
data Target
  = SubscriptTrg Target' Expr'
  | NameTrg VarName'
  | TupleTrg [Target']
  deriving (Eq, Ord, Show, Read)

type Target' = WithLoc' Target

-- | `CmpOp'` is a type for comparision operators.
-- This is annotated with its type as let-polymorphism.
data CmpOp' = CmpOp' CmpOp Type
  deriving (Eq, Ord, Show, Read)

data Comprehension = Comprehension Target' Expr' (Maybe Expr')
  deriving (Eq, Ord, Show, Read)

-- | `Expr` represents the exprs of our restricted Python-like language.
--
-- \[
--     \begin{array}{rl}
--         e ::= & e \operatorname{boolop} e \\
--         \vert & e \operatorname{binop} e \\
--         \vert & \operatorname{unaryop} e \\
--         \vert & \lambda x _ \tau x _ \tau \dots x _ \tau. e \\
--         \vert & \mathbf{if}~ e ~\mathbf{then}~ e ~\mathbf{else}~ e \\
--         \vert & \lbrack e ~\mathbf{for}~ y ~\mathbf{in}~ e ~(\mathbf{if}~ e)? \rbrack \\
--         \vert & e \operatorname{cmpop} e \\
--         \vert & e (e, e, \dots, e) \\
--         \vert & \operatorname{constant} \\
--         \vert & e \lbrack e \rbrack \\
--         \vert & x \\
--         \vert & \lbrack e, e, \dots, e \rbrack _ \tau \\
--         \vert & e \lbrack e? \colon e? \colon e? \rbrack \\
--     \end{array}
-- \]
data Expr
  = BoolOp Expr' BoolOp Expr'
  | BinOp Expr' Operator Expr'
  | UnaryOp UnaryOp Expr'
  | Lambda [(VarName', Type)] Expr'
  | IfExp Expr' Expr' Expr'
  | ListComp Expr' Comprehension
  | Compare Expr' CmpOp' Expr'
  | Call Expr' [Expr']
  | Constant Constant
  | Attribute Expr' Attribute'
  | Subscript Expr' Expr'
  | Starred Expr'
  | Name VarName'
  | List Type [Expr']
  | Tuple [Expr']
  | SubscriptSlice Expr' (Maybe Expr') (Maybe Expr') (Maybe Expr')
  deriving (Eq, Ord, Show, Read)

type Expr' = WithLoc' Expr

-- | `Statement` represents the statements of our restricted Python-like language.
-- They appear in bodies of `def`.
--
-- \[
--     \begin{array}{rl}
--         \mathrm{stmt} ::= & \mathbf{return}~ e \\
--         \vert & y \operatorname{binop} = e \\
--         \vert & y _ \tau := e \\
--         \vert & \mathbf{for}~ y ~\mathbf{in}~ e \colon\quad \mathrm{stmt}; \mathrm{stmt}; \dots; \mathrm{stmt} \\
--         \vert & \mathbf{if}~ e \colon\quad \mathrm{stmt}; \mathrm{stmt}; \dots; \mathrm{stmt};\quad \mathbf{else}\colon\quad \mathrm{stmt}; \mathrm{stmt}; \dots; \mathrm{stmt} \\
--         \vert & \mathbf{assert}~ e \\
--     \end{array}
-- \]
data Statement
  = Return Expr'
  | AugAssign Target' Operator Expr'
  | AnnAssign Target' Type Expr'
  | For Target' Expr' [Statement]
  | If Expr' [Statement] [Statement]
  | Assert Expr'
  | -- | expression statements
    Expr' Expr'
  deriving (Eq, Ord, Show, Read)

pattern Append loc t e1 e2 <- Expr' (WithLoc' loc (Call (WithLoc' _ (Attribute e1 (WithLoc' _ (BuiltinAppend t)))) [e2]))

-- | `TopLevelStatement` represents the statements of our restricted Python-like language.
-- They appear in the toplevel of programs.
--
-- \[
--     \begin{array}{rl}
--         \mathrm{tlstmt} ::= & x _ \tau := e \\
--         \vert & \mathbf{def}~ x (x _ \tau, x _ \tau, \dots, x _ \tau) \to \tau \colon\quad \mathrm{stmt}; \mathrm{stmt}; \dots; \mathrm{stmt} \\
--         \vert & \mathbf{assert}~ e \\
--     \end{array}
-- \]
data ToplevelStatement
  = ToplevelAnnAssign VarName' Type Expr'
  | ToplevelFunctionDef VarName' [(VarName', Type)] Type [Statement]
  | ToplevelAssert Expr'
  deriving (Eq, Ord, Show, Read)

-- | `Program` represents the programs of our restricted Python-like language.
--
-- \[
--     \begin{array}{rl}
--         \mathrm{prog} ::= & \mathrm{tlstmt}; \mathrm{tlstmt}; \dots; \mathrm{tlstmt} \\
--     \end{array}
-- \]
type Program = [ToplevelStatement]
