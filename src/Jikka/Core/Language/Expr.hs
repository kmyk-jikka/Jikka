{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Jikka.Core.Language.Expr
-- Description : has data types of our core language. / core 言語のためのデータ型を持ちます。
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
--         \vert & \tau \times \tau \times \dots \times \tau \\
--         \vert & \tau \to \tau
--     \end{array}
-- \]
data Type
  = VarTy TypeName
  | IntTy
  | BoolTy
  | ListTy Type
  | TupleTy [Type]
  | FunTy Type Type
  deriving (Eq, Ord, Show, Read)

-- | TODO: What is the difference between `Literal` and `Builtin`?
data Builtin
  = -- arithmetical functions

    -- | \(: \int \to \int\)
    Negate
  | -- | \(: \int \to \int \to \int\)
    Plus
  | -- | \(: \int \to \int \to \int\)
    Minus
  | -- | \(: \int \to \int \to \int\)
    Mult
  | -- | \(: \int \to \int \to \int\)
    FloorDiv
  | -- | \(: \int \to \int \to \int\)
    FloorMod
  | -- | \(: \int \to \int \to \int\)
    CeilDiv
  | -- | \(: \int \to \int \to \int\)
    CeilMod
  | -- | \(: \int \to \int \to \int\)
    Pow
  | -- advanced arithmetical functions

    -- | \(: \int \to \int\)
    Abs
  | -- | \(: \int \to \int \to \int\)
    Gcd
  | -- | \(: \int \to \int \to \int\)
    Lcm
  | -- | \(: \forall \alpha. \alpha \to \alpha \to \alpha\)
    Min2 Type
  | -- | \(: \forall \alpha. \alpha \to \alpha \to \alpha\)
    Max2 Type
  | -- | iterated application \((\lambda k f x. f^k(x)): \forall \alpha. \int \to (\alpha \to \alpha) \to \alpha \to \alpha\)
    Iterate Type
  | -- logical functions

    -- | \(: \bool \to \bool\)
    Not
  | -- | \(: \bool \to \bool \to \bool\)
    And
  | -- | \(: \bool \to \bool \to \bool\)
    Or
  | -- | \(: \bool \to \bool \to \bool\)
    Implies
  | -- | \(: \forall \alpha. \bool \to \alpha \to \alpha \to \alpha\)
    If Type
  | -- bitwise functions

    -- | \(: \int \to \int\)
    BitNot
  | -- | \(: \int \to \int \to \int\)
    BitAnd
  | -- | \(: \int \to \int \to \int\)
    BitOr
  | -- | \(: \int \to \int \to \int\)
    BitXor
  | -- | \(: \int \to \int \to \int\)
    BitLeftShift
  | -- | \(: \int \to \int \to \int\)
    BitRightShift
  | -- matrix functions

    -- | matrix application \(: \int^{H \times W} \to \int^W \to \int^H\)
    MatAp Int Int
  | -- | zero matrix \(: \to \int^{n \times n}\)
    MatZero Int
  | -- | unit matrix \(: \to \int^{n \times n}\)
    MatOne Int
  | -- | matrix addition \(: \int^{H \times W} \to \int^{H \times W} \to \int^{H \times W}\)
    MatAdd Int Int
  | -- | matrix multiplication \(: \int^{H \times n} \to \int^{n \times W} \to \int^{H \times W}\)
    MatMul Int Int Int
  | -- | matrix power \(: \int^{n \times n} \to \int \to \int^{n \times n}\)
    MatPow Int
  | -- | vector point-wise floor-mod \(: \int^{n} \to \int \to \int^{n}\)
    VecFloorMod Int
  | -- | matrix point-wise floor-mod \(: \int^{H \times W} \to \int \to \int^{H \times W}\)
    MatFloorMod Int Int
  | -- modular functions

    -- | \(: \int \to \int \to \int\)
    ModNegate
  | -- | \(: \int \to \int \to \int \to \int\)
    ModPlus
  | -- | \(: \int \to \int \to \int \to \int\)
    ModMinus
  | -- | \(: \int \to \int \to \int \to \int\)
    ModMult
  | -- | \(: \int \to \int \to \int\)
    ModInv
  | -- | \(: \int \to \int \to \int \to \int\)
    ModPow
  | -- | matrix application \(: \int^{H \times W} \to \int^W \to \int \to \int^H\)
    ModMatAp Int Int
  | -- | matrix addition \(: \int^{H \times W} \to \int^{H \times W} \to \int \to \int^{H \times W}\)
    ModMatAdd Int Int
  | -- | matrix multiplication \(: \int^{H \times n} \to \int^{n \times W} \to \int \to \int^{H \times W}\)
    ModMatMul Int Int Int
  | -- | matrix power \(: \int^{n \times n} \to \int \to \int^{n \times n}\)
    ModMatPow Int
  | -- list functions

    -- | \(: \forall \alpha. \alpha \to \list(\alpha) \to \list(\alpha)\)
    Cons Type
  | -- | \(: \forall \alpha \beta. (\beta \to \alpha \to \beta) \to \beta \to \list(\alpha) \to \beta\)
    Foldl Type Type
  | -- | \(: \forall \alpha \beta. (\beta \to \alpha \to \beta) \to \beta \to \list(\alpha) \to \list(\beta)\)
    Scanl Type Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \int\)
    Len Type
  | -- | \(: \forall \alpha \beta. (\alpha \to \beta) \to \list(\alpha) \to \list(\beta)\)
    Map Type Type
  | -- | \(: \forall \alpha \beta. (\alpha \to \bool) \to \list(\alpha) \to \list(\beta)\)
    Filter Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \int \to \alpha\)
    At Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \int \to \alpha \to \list(\alpha)\)
    SetAt Type
  | -- | \(: \forall \alpha. \alpha \to \list(\alpha) \to \bool\)
    Elem Type
  | -- | \(: \list(\int) \to \int\)
    Sum
  | -- | \(: \list(\int) \to \int\)
    Product
  | -- | \(: \list(\int) \to \int \to \int\)
    ModSum
  | -- | \(: \list(\int) \to \int \to \int\)
    ModProduct
  | -- | \(: \forall \alpha. \list(\alpha) \to \alpha\)
    Min1 Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \alpha\)
    Max1 Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \int\)
    ArgMin Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \int\)
    ArgMax Type
  | -- | \(: \list(\bool) \to \bool\)
    All
  | -- | \(: \list(\bool) \to \bool\)
    Any
  | -- | \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
    Sorted Type
  | -- | \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
    Reversed Type
  | -- | \(: \int \to \list(\int)\)
    Range1
  | -- | \(: \int \to \int \to \list(\int)\)
    Range2
  | -- | \(: \int \to \int \to \int \to \list(\int)\)
    Range3
  | -- tuple functions

    -- | \(: \forall \alpha_0 \alpha_1 \dots \alpha _ {n - 1}. \alpha_0 \to \dots \to \alpha _ {n - 1} \to \alpha_0 \times \dots \times \alpha _ {n - 1}\)
    Tuple [Type]
  | -- | \(: \forall \alpha_0 \alpha_1 \dots \alpha _ {n - 1}. \alpha_0 \times \dots \times \alpha _ {n - 1} \to \alpha_i\)
    Proj [Type] Int
  | -- comparison

    -- | \(: \forall \alpha. \alpha \to \alpha \to \bool\)
    LessThan Type
  | -- | \(: \forall \alpha. \alpha \to \alpha \to \bool\)
    LessEqual Type
  | -- | \(: \forall \alpha. \alpha \to \alpha \to \bool\)
    GreaterThan Type
  | -- | \(: \forall \alpha. \alpha \to \alpha \to \bool\)
    GreaterEqual Type
  | -- | \(: \forall \alpha. \alpha \to \alpha \to \bool\)
    Equal Type
  | -- | \(: \forall \alpha. \alpha \to \alpha \to \bool\)
    NotEqual Type
  | -- combinational functions

    -- | \(: \int \to \int\)
    Fact
  | -- | \(: \int \to \int \to \int\)
    Choose
  | -- | \(: \int \to \int \to \int\)
    Permute
  | -- | \(: \int \to \int \to \int\)
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
  | -- | \(: \bot : \forall \alpha. \alpha\). The second argument is its error message.
    LitBottom Type String
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
    App Expr Expr
  | -- | The lambdas are also not curried.
    Lam VarName Type Expr
  | -- | This "let" is not recursive.
    Let VarName Type Expr Expr
  deriving (Eq, Ord, Show, Read)

pattern Fun2Ty t1 t2 ret = FunTy t1 (FunTy t2 ret)

pattern Fun3Ty t1 t2 t3 ret = FunTy t1 (FunTy t2 (FunTy t3 ret))

pattern Fun1STy t <-
  (\case FunTy t1 ret | t1 == ret -> Just ret; _ -> Nothing -> Just t)
  where
    Fun1STy t = FunTy t t

pattern Fun2STy t <-
  (\case Fun2Ty t1 t2 ret | t1 == ret && t2 == ret -> Just ret; _ -> Nothing -> Just t)
  where
    Fun2STy t = Fun2Ty t t t

pattern Fun3STy t <-
  (\case Fun3Ty t1 t2 t3 ret | t1 == ret && t2 == ret && t3 == ret -> Just ret; _ -> Nothing -> Just t)
  where
    Fun3STy t = Fun3Ty t t t t

pattern FunLTy t <-
  (\case FunTy (ListTy t1) ret | t1 == ret -> Just ret; _ -> Nothing -> Just t)
  where
    FunLTy t = FunTy (ListTy t) t

vectorTy :: Int -> Type
vectorTy n = TupleTy (replicate n IntTy)

matrixTy :: Int -> Int -> Type
matrixTy h w = TupleTy (replicate h (TupleTy (replicate w IntTy)))

pattern UnitTy = TupleTy []

pattern LitInt' n = Lit (LitInt n)

pattern Lit0 = Lit (LitInt 0)

pattern Lit1 = Lit (LitInt 1)

pattern Lit2 = Lit (LitInt 2)

pattern LitMinus1 = Lit (LitInt (-1))

pattern LitBool' p = Lit (LitBool p)

pattern LitTrue = Lit (LitBool True)

pattern LitFalse = Lit (LitBool False)

pattern Builtin builtin = Lit (LitBuiltin builtin)

pattern App2 f e1 e2 = App (App f e1) e2

pattern App3 f e1 e2 e3 = App (App (App f e1) e2) e3

pattern App4 f e1 e2 e3 e4 = App (App (App (App f e1) e2) e3) e4

pattern AppBuiltin builtin e1 = App (Lit (LitBuiltin builtin)) e1

pattern AppBuiltin2 builtin e1 e2 = App2 (Lit (LitBuiltin builtin)) e1 e2

pattern AppBuiltin3 builtin e1 e2 e3 = App3 (Lit (LitBuiltin builtin)) e1 e2 e3

pattern Lam2 x1 t1 x2 t2 e = Lam x1 t1 (Lam x2 t2 e)

pattern Lam3 x1 t1 x2 t2 x3 t3 e = Lam x1 t1 (Lam x2 t2 (Lam x3 t3 e))

pattern LamId x t <-
  (\case Lam x t (Var y) | x == y -> Just (x, t); _ -> Nothing -> Just (x, t))
  where
    LamId x t = Lam x t (Var x)

-- | `ToplevelExpr` is the toplevel exprs. In our core, "let rec" is allowed only on the toplevel.
--
-- \[
--     \begin{array}{rl}
--         \mathrm{tle} ::= & e \\
--         \vert & \mathbf{let}~ x: \tau = e ~\mathbf{in}~ \mathrm{tle} \\
--         \vert & \mathbf{let~rec}~ x(x: \tau, x: \tau, \dots, x: \tau): \tau = e ~\mathbf{in}~ \mathrm{tle}
--     \end{array}
-- \]
data ToplevelExpr
  = ResultExpr Expr
  | ToplevelLet VarName Type Expr ToplevelExpr
  | ToplevelLetRec VarName [(VarName, Type)] Type Expr ToplevelExpr
  deriving (Eq, Ord, Show, Read)

type Program = ToplevelExpr
