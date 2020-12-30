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

-- | `Type` represents the types of our core language. This is similar to the `Type` of GHC Core.
-- See also [commentary/compiler/type-type](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/type-type).
--
-- \[
--     \begin{array}{rl}
--         \tau ::= & \mathbf{int} \\
--         \vert & \mathbf{bool} \\
--         \vert & \mathbf{list}(\tau) \\
--         \vert & \tau_0 \times \tau_1 \times \dots \times \tau_{n-1} \to \tau_n
--     \end{array}
-- \]
data Type
  = IntTy
  | BoolTy
  | ListTy Type
  | -- | The functions are not curried.
    FunTy [Type] Type
  deriving (Eq, Ord, Show, Read)

data Builtin
  = -- arithmetical functions
    Negate
  | Plus
  | Minus
  | Mult
  | FloorDiv
  | FloorMod
  | CeilDiv
  | CeilMod
  | Pow
  | -- induction functions
    NatInd Type
  | -- advanced arithmetical functions
    Abs
  | Gcd
  | Lcm
  | Min
  | Max
  | -- logical functions
    Not
  | And
  | Or
  | Implies
  | If Type
  | -- bitwise functions
    BitNot
  | BitAnd
  | BitOr
  | BitXor
  | BitLeftShift
  | BitRightShift
  | -- modular functions
    Inv
  | PowMod
  | -- list functions
    Len Type
  | Tabulate Type
  | Map Type Type
  | At Type
  | Sum
  | Product
  | Min1
  | Max1
  | ArgMin
  | ArgMax
  | All
  | Any
  | Sorted Type
  | List Type
  | Reversed Type
  | Range1
  | Range2
  | Range3
  | -- arithmetical relations
    LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | -- equality relations (polymorphic)
    Equal Type
  | NotEqual Type
  | -- combinational functions
    Fact
  | Choose
  | Permute
  | MultiChoose
  deriving (Eq, Ord, Show, Read)

data Literal
  = LitBuiltin Builtin
  | LitInt Integer
  | LitBool Bool
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

data RecKind
  = NonRec
  | Rec
  deriving (Eq, Ord, Show, Read)

-- | `ToplevelExpr` is the toplevel exprs. In our core, "let rec" is allowed only on the toplevel.
data ToplevelExpr
  = ResultExpr Expr
  | ToplevelLet RecKind VarName [(VarName, Type)] Type Expr ToplevelExpr
  deriving (Eq, Ord, Show, Read)

type Program = ToplevelExpr
