{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Jikka.RestrictedPython.Language.Expr
-- Description : contains data types of the restricted Python.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Language.Expr
  ( Ident (..),
    unIdent,
    Type (..),
    Constant (..),
    Target (..),
    Comprehension (..),
    Expr (..),
    Statement (..),
    ToplevelStatement (..),
    Program,
    BoolOp (..),
    CmpOp (..),
    Operator (..),
    UnaryOp (..),
  )
where

import Data.String (IsString)
import Jikka.Python.Language.Expr (BoolOp (..), CmpOp (..), Operator (..), UnaryOp (..))

newtype Ident = Ident String deriving (Eq, Ord, Show, Read, IsString)

unIdent :: Ident -> String
unIdent (Ident x) = x

-- | `Type` represents the types of our restricted Python-like language.
--
-- \[
--     \begin{array}{rl}
--         \tau ::= & \alpha \\
--         \vert & \mathbf{int} \\
--         \vert & \mathbf{bool} \\
--         \vert & \mathbf{list}(\tau) \\
--         \vert & \tau \times \tau \times \dots \times \tau \\
--         \vert & \tau \times \tau \times \dots \times \tau \to \tau
--     \end{array}
-- \]
--
-- NOTE: \(\mathbf{None}\) is represented as the 0-tuple.
data Type
  = VarTy Ident
  | IntTy
  | BoolTy
  | ListTy Type
  | TupleTy [Type]
  | CallableTy [Type] Type
  deriving (Eq, Ord, Show, Read)

data Constant
  = ConstNone
  | ConstInt Integer
  | ConstBool Bool
  deriving (Eq, Ord, Show, Read)

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
  = SubscriptTrg Target Expr
  | NameTrg Ident
  | TupleTrg [Target]
  deriving (Eq, Ord, Show, Read)

data Comprehension = Comprehension Target Expr (Maybe Expr)
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
  = BoolOp Expr BoolOp Expr
  | BinOp Expr Operator Expr
  | UnaryOp UnaryOp Expr
  | Lambda [(Ident, Type)] Expr
  | IfExp Expr Expr Expr
  | ListComp Expr Comprehension
  | Compare Expr CmpOp Expr
  | Call Expr [Expr]
  | Constant Constant
  | Subscript Expr Expr
  | Name Ident
  | List Type [Expr]
  | Tuple [Expr]
  | SubscriptSlice Expr (Maybe Expr) (Maybe Expr) (Maybe Expr)
  deriving (Eq, Ord, Show, Read)

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
  = Return Expr
  | AugAssign Target Operator Expr
  | AnnAssign Target Type Expr
  | For Target Expr [Statement]
  | If Expr [Statement] [Statement]
  | Assert Expr
  deriving (Eq, Ord, Show, Read)

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
  = ToplevelAnnAssign Ident Type Expr
  | ToplevelFunctionDef Ident [(Ident, Type)] Type [Statement]
  | ToplevelAssert Expr
  deriving (Eq, Ord, Show, Read)

-- | `Program` represents the programs of our restricted Python-like language.
--
-- \[
--     \begin{array}{rl}
--         \mathrm{prog} ::= & \mathrm{tlstmt}; \mathrm{tlstmt}; \dots; \mathrm{tlstmt} \\
--     \end{array}
-- \]
type Program = [ToplevelStatement]
