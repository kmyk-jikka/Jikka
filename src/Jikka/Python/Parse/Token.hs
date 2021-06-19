-- |
-- Module      : Jikka.Core.Parse.Token
-- Description : defines tokens of the standard Python with Alex.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Python.Parse.Token where

import Data.Int (Int8)
import Jikka.Common.Location

data CmpOp
  = DoubleEqual
  | NotEqual
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  deriving (Eq, Ord, Show, Read)

data DivModOp
  = Div
  | FloorDiv
  | FloorMod
  | CeilDiv
  | CeilMod
  deriving (Eq, Ord, Show, Read)

data AugOp
  = AugAdd
  | AugSub
  | AugMul
  | AugAt
  | AugDiv
  | AugFloorDiv
  | AugFloorMod
  | AugCeilDiv
  | AugCeilMod
  | AugPow
  | AugBitRShift
  | AugBitLShift
  | AugBitAnd
  | AugBitXor
  | AugBitOr
  | AugMin
  | AugMax
  deriving (Eq, Ord, Show, Read)

-- | We don't have to classify tokens in detail, but it's convenient for testing and debugging.
data Token
  = -- literals
    None
  | Int Integer
  | Bool Bool
  | String String
  | Bytes [Int8]
  | Float Double
  | Imaginary Double
  | -- keywords
    Def
  | If
  | Elif
  | Else
  | For
  | In
  | Assert
  | Return
  | Lambda
  | -- punctuations
    Arrow
  | Colon
  | Semicolon
  | Comma
  | Dot
  | Equal
  | Underscore
  | -- parens
    OpenBrace
  | OpenBracket
  | OpenParen
  | CloseBrace
  | CloseBracket
  | CloseParen
  | -- identifier
    Ident String
  | -- operators
    WalrusOp
  | ImpliesOp
  | OrOp
  | AndOp
  | NotOp
  | CmpOp CmpOp
  | MinOp
  | MaxOp
  | BitOrOp
  | BitXorOp
  | BitAndOp
  | BitLShiftOp
  | BitRShiftOp
  | PlusOp
  | MinusOp
  | MulOp
  | DivModOp DivModOp
  | AtOp
  | BitNotOp
  | PowOp
  | AugOp AugOp
  | -- indent
    Newline
  | Indent
  | Dedent
  | -- reserved keywords
    As
  | Async
  | Await
  | Break
  | Class
  | Continue
  | Del
  | Except
  | Finally
  | From
  | Global
  | Import
  | Is
  | Nonlocal
  | Pass
  | Raise
  | Try
  | While
  | With
  | Yield
  deriving (Eq, Ord, Show, Read)

type Token' = WithLoc Token
