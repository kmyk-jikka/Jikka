{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Jikka.Core.Language.Expr
-- Description : contains data types of our core language.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Core.Language.Expr` module has the basic data types for our core language.
-- They are similar to the GHC Core language.
module Jikka.Core.Language.Expr where

import Data.String (IsString)

newtype VarName = VarName String deriving (Eq, Ord, Show, Read, IsString)

unVarName :: VarName -> String
unVarName (VarName name) = name

newtype TypeName = TypeName String deriving (Eq, Ord, Show, Read, IsString)

unTypeName :: TypeName -> String
unTypeName (TypeName name) = name

-- | `Type` represents the types of our core language. This is similar to the `Type` of GHC Core.
-- See also [commentary/compiler/type-type](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/type-type).
--
-- \[
--     \newcommand\int{\mathbf{int}}
--     \newcommand\bool{\mathbf{bool}}
--     \newcommand\list{\mathbf{list}}
--     \begin{array}{rl}
--         \tau ::= & \alpha \\
--         \vert & \int \\
--         \vert & \bool \\
--         \vert & \list(\tau) \\
--         \vert & \tau_0 \times \tau_1 \times \dots \times \tau_{n-1} \\
--         \vert & \tau_0 \times \tau_1 \times \dots \times \tau_{n-1} \to \tau_n
--     \end{array}
-- \]
data Type
  = VarTy TypeName
  | IntTy
  | BoolTy
  | ListTy Type
  | TupleTy [Type]
  | -- | The functions are not curried. TODO: currying?
    FunTy [Type] Type
  deriving (Eq, Ord, Show, Read)

-- | TODO: What is the difference between `Literal` and `Builtin`?
data Builtin
  = -- arithmetical functions

    -- | \(: \int \to \int\)
    Negate
  | -- | \(: \int \times \int \to \int\)
    Plus
  | -- | \(: \int \times \int \to \int\)
    Minus
  | -- | \(: \int \times \int \to \int\)
    Mult
  | -- | \(: \int \times \int \to \int\)
    FloorDiv
  | -- | \(: \int \times \int \to \int\)
    FloorMod
  | -- | \(: \int \times \int \to \int\)
    CeilDiv
  | -- | \(: \int \times \int \to \int\)
    CeilMod
  | -- | \(: \int \times \int \to \int\)
    Pow
  | -- induction functions

    -- | natural induction \(: \forall \alpha. \alpha \times (\alpha \to \alpha) \times \int \to \alpha\)
    NatInd Type
  | -- advanced arithmetical functions

    -- | \(: \int \to \int\)
    Abs
  | -- | \(: \int \times \int \to \int\)
    Gcd
  | -- | \(: \int \times \int \to \int\)
    Lcm
  | -- | \(: \forall \alpha. \alpha \times \alpha \to \alpha\)
    Min2 Type
  | -- | \(: \forall \alpha. \alpha \times \alpha \to \alpha\)
    Max2 Type
  | -- logical functions

    -- | \(: \bool \to \bool\)
    Not
  | -- | \(: \bool \times \bool \to \bool\)
    And
  | -- | \(: \bool \times \bool \to \bool\)
    Or
  | -- | \(: \bool \times \bool \to \bool\)
    Implies
  | -- | \(: \forall \alpha. \bool \times \alpha \times \alpha \to \alpha\)
    If Type
  | -- bitwise functions

    -- | \(: \int \to \int\)
    BitNot
  | -- | \(: \int \times \int \to \int\)
    BitAnd
  | -- | \(: \int \times \int \to \int\)
    BitOr
  | -- | \(: \int \times \int \to \int\)
    BitXor
  | -- | \(: \int \times \int \to \int\)
    BitLeftShift
  | -- | \(: \int \times \int \to \int\)
    BitRightShift
  | -- modular functions

    -- | \(: \int \times \int \to \int\)
    ModInv
  | -- | \(: \int \times \int \times \int \to \int\)
    ModPow
  | -- list functions

    -- | \(: \forall \alpha. \alpha \times \list(\alpha) \to \list(\alpha)\)
    Cons Type
  | -- | \(: \foall \alpha \beta. (\beta \times \alpha \to \beta) \times \beta \times \list(\alpha) \to \beta\)
    Foldl Type Type
  | -- | \(: \foall \alpha \beta. (\beta \times \alpha \to \beta) \times \beta \times \list(\alpha) \to \list(\beta)\)
    Scanl Type Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \int\)
    Len Type
  | -- | \(: \forall \alpha. \int \times (\int \to \alpha) \to \list(\alpha)\)
    Tabulate Type
  | -- | \(: \forall \alpha \beta. (\alpha \to \beta) \times \list(\alpha) \to \list(\beta)\)
    Map Type Type
  | -- | \(: \forall \alpha \beta. (\alpha \to \bool) \times \list(\alpha) \to \list(\beta)\)
    Filter Type
  | -- | \(: \forall \alpha. \list(\alpha) \times \int \to \alpha\)
    At Type
  | -- | \(: \forall \alpha. \list(alpha) \times \int \times \alpha \to \list(\alpha)\)
    SetAt Type
  | -- | \(: \forall \alpha. \alpha \times \list(\alpha) \to \bool\)
    Elem Type
  | -- | \(: \list(\int) \to \int\)
    Sum
  | -- | \(: \list(\int) \to \int\)
    Product
  | -- | \(: \forall \alpha. \list(\alpha) \to \alpha\)
    Min1 Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \alpha\)
    Max1 Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \alpha\)
    ArgMin Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \alpha\)
    ArgMax Type
  | -- | \(: \list(\bool) \to \bool\)
    All
  | -- | \(: \list(\bool) \to \bool\)
    Any
  | -- | \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
    Sorted Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
    List Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
    Reversed Type
  | -- | \(: \int \to \list(\int)\)1
    Range1
  | -- | \(: \int \times \int \to \list(\int)\)1
    Range2
  | -- | \(: \int \times \int \times \int \to \list(\int)\)1
    Range3
  | -- tuple functions

    -- | \(: \forall \alpha_0 \alpha_1 \dots \alpha _ {n - 1}. \alpha_0 \times \dots \times \alpha _ {n - 1} \to \alpha_0 \times \dots \times \alpha _ {n - 1}\)
    Tuple [Type]
  | -- | \(: \forall \alpha_0 \alpha_1 \dots \alpha _ {n - 1}. \alpha_0 \times \dots \times \alpha _ {n - 1} \to \alpha_i\)
    Proj [Type] Int
  | -- comparison

    -- | \(: \forall \alpha. \alpha \times \alpha \to \alpha\)
    LessThan Type
  | -- | \(: \forall \alpha. \alpha \times \alpha \to \alpha\)
    LessEqual Type
  | -- | \(: \forall \alpha. \alpha \times \alpha \to \alpha\)
    GreaterThan Type
  | -- | \(: \forall \alpha. \alpha \times \alpha \to \alpha\)
    GreaterEqual Type
  | -- | \(: \forall \alpha. \alpha \times \alpha \to \bool\)
    Equal Type
  | -- | \(: \forall \alpha. \alpha \times \alpha \to \bool\)
    NotEqual Type
  | -- combinational functions

    -- | \(: \int \to \int\)
    Fact
  | -- | \(: \int \times \int \to \int\)
    Choose
  | -- | \(: \int \times \int \to \int\)
    Permute
  | -- | \(: \int \times \int \to \int\)
    MultiChoose
  deriving (Eq, Ord, Show, Read)

data Literal
  = LitBuiltin Builtin
  | -- | \(: \forall \alpha. \int\)
    LitInt Integer
  | -- | \(: \forall \alpha. \bool\)
    LitBool Bool
  | -- | \(: \forall \alpha. \list(\alpha)\)
    LitNil Type
  deriving (Eq, Ord, Show, Read)

-- | `Expr` represents the exprs of our core language. This is similar to the `Expr` of GHC Core.
-- See also [commentary/compiler/core-syn-type](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/core-syn-type).
--
-- \[
--     \begin{array}{rl}
--         e ::= & x \\
--         \vert & \mathrm{literal}\ldots \\
--         \vert & e_0(e_1, e_2, \dots, e_n) \\
--         \vert & \lambda ~ x_0\colon \tau_0, x_1\colon \tau_1, \dots, x_{n-1}\colon \tau_{n-1}. ~ e \\
--         \vert & \mathbf{let} ~ x\colon \tau = e_1 ~ \mathbf{in} ~ e_2
--     \end{array}
-- \]
data Expr
  = Var VarName
  | Lit Literal
  | -- | The functions are not curried.
    App Expr [Expr]
  | -- | The lambdas are also not curried.
    Lam [(VarName, Type)] Expr
  | -- | This "let" is not recursive.
    Let VarName Type Expr Expr
  deriving (Eq, Ord, Show, Read)

pattern Fun1Ty t <-
  (\case FunTy [t1] t0 | t1 == t0 -> Just t0; _ -> Nothing -> Just t)
  where
    Fun1Ty t = FunTy [t] t

pattern Fun2Ty t <-
  (\case FunTy [t1, t2] t0 | t1 == t0 && t2 == t0 -> Just t0; _ -> Nothing -> Just t)
  where
    Fun2Ty t = FunTy [t, t] t

pattern Fun3Ty t <-
  (\case FunTy [t1, t2, t3] t0 | t1 == t0 && t2 == t0 && t3 == t0 -> Just t0; _ -> Nothing -> Just t)
  where
    Fun3Ty t = FunTy [t, t, t] t

pattern FunLTy t <-
  (\case FunTy [ListTy t1] t0 | t1 == t0 -> Just t0; _ -> Nothing -> Just t)
  where
    FunLTy t = FunTy [ListTy t] t

pattern Lit0 = Lit (LitInt 0)

pattern Lit1 = Lit (LitInt 1)

pattern Lit2 = Lit (LitInt 2)

pattern LitMinus1 = Lit (LitInt (-1))

pattern LitTrue = Lit (LitBool True)

pattern LitFalse = Lit (LitBool False)

pattern Builtin builtin = Lit (LitBuiltin builtin)

pattern AppBuiltin builtin args = App (Lit (LitBuiltin builtin)) args

pattern Lam1 x1 t1 e = Lam [(x1, t1)] e

pattern Lam2 x1 t1 x2 t2 e = Lam [(x1, t1), (x2, t2)] e

pattern Lam3 x1 t1 x2 t2 x3 t3 e = Lam [(x1, t1), (x2, t2), (x3, t3)] e

pattern LamId x t <-
  (\case Lam [(x, t)] (Var y) | x == y -> Just (x, t); _ -> Nothing -> Just (x, t))
  where
    LamId x t = Lam [(x, t)] (Var x)

-- | `ToplevelExpr` is the toplevel exprs. In our core, "let rec" is allowed only on the toplevel.
--
-- \[
--     \begin{array}{rl}
--         \mathrm{tle} ::= & e \\
--         \vert & \mathbf{let}~ x: \tau = e ~\mathbf{in}~ \mathrm{tle} \\
--         \vert & \mathbf{letrec}~ x(x: \tau, x: \tau, \dots, x: \tau): \tau = e ~\mathbf{in}~ \mathrm{tle}
--     \end{array}
-- \]
data ToplevelExpr
  = ResultExpr Expr
  | ToplevelLet VarName Type Expr ToplevelExpr
  | ToplevelLetRec VarName [(VarName, Type)] Type Expr ToplevelExpr
  deriving (Eq, Ord, Show, Read)

type Program = ToplevelExpr
