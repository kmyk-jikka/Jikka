-- |
-- Module      : Jikka.Core.Parse.Token
-- Description : defines tokens of our core language. / core 言語の字句要素を定義します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Parse.Token where

import Jikka.Common.Location

data Operator
  = -- arithmetic operators
    Plus
  | Minus
  | Mult
  | FloorDiv
  | FloorMod
  | CeilDiv
  | CeilMod
  | Pow
  | -- boolean operators
    Not
  | And
  | Or
  | Implies
  | -- bit operators
    BitNot
  | BitAnd
  | BitOr
  | BitXor
  | BitLShift
  | BitRShift
  | -- comparators
    DoubleEqual
  | NotEqual
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  deriving (Eq, Ord, Show, Read)

-- | We don't have to classify tokens in detail, but it's convenient for testing and debugging.
data Token
  = -- identifier
    Ident String
  | -- literals
    Int Integer
  | Bool Bool
  | String String
  | -- keywords
    Let
  | Rec
  | In
  | If
  | Then
  | Else
  | Fun
  | Dot
  | Assert
  | Forall
  | -- punctuations
    Arrow
  | Equal
  | Colon
  | Comma
  | Underscore
  | BackArrow
  | At
  | -- parens
    OpenBracket
  | OpenParen
  | CloseBracket
  | CloseParen
  | -- operators
    Operator Operator
  deriving (Eq, Ord, Show, Read)

type Token' = WithLoc Token
