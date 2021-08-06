-- |
-- Module      : Jikka.CPlusPlus.Parse.Token
-- Description : defines tokens of our C++ language. / C++ 言語の字句要素を定義します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.CPlusPlus.Parse.Token where

import Jikka.Common.Location

data Operator
  = -- arithmetic operators
    Plus
  | Minus
  | Mult
  | Div
  | Mod
  | -- augumented arithmetic operators
    PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign
  | ModAssign
  | -- boolean operators
    LogicalNot
  | LogicalAnd
  | LogicalOr
  | -- bit operators
    BitNot
  | BitAnd
  | BitOr
  | BitXor
  | BitLShift
  | BitRShift
  | -- augumented bit operators
    BitAndAssign
  | BitOrAssign
  | BitXorAssign
  | BitLShiftAssign
  | BitRShiftAssign
  | -- comparators
    DoubleEqual
  | NotEqual
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  deriving (Eq, Ord, Show, Read)

-- | The keywords of C++. See <https://en.cppreference.com/w/cpp/keyword>.
data Keyword
  = Alignas
  | Alignof
  | And
  | AndEq
  | Asm
  | AtomicCancel
  | AtomicCommit
  | AtomicNoexcept
  | Auto
  | Bitand
  | Bitor
  | Bool'
  | Break
  | Case
  | Catch
  | Char'
  | Char8T
  | Char16T
  | Char32T
  | Class
  | Compl
  | Concept
  | Const
  | Consteval
  | Constexpr
  | Constinit
  | ConstCast
  | Continue
  | CoAwait
  | CoReturn
  | CoYield
  | Decltype
  | Default
  | Delete
  | Do
  | Double
  | DynamicCast
  | Else
  | Enum
  | Explicit
  | Export
  | Extern
  | False'
  | Float
  | For
  | Friend
  | Goto
  | If
  | Inline
  | Int'
  | Long'
  | Mutable
  | Namespace
  | New
  | Noexcept
  | Not
  | NotEq
  | Nullptr
  | Operator'
  | Or
  | OrEq
  | Private
  | Protected
  | Public
  | Reflexpr
  | Register
  | ReinterpretCast
  | Requires
  | Return
  | Short
  | Signed
  | Sizeof
  | Static
  | StaticAssert
  | StaticCast
  | Struct
  | Switch
  | Synchronized
  | Template
  | This
  | ThreadLocal
  | Throw
  | True'
  | Try
  | Typedef
  | Typeid
  | Typename
  | Union
  | Unsigned
  | Using
  | Virtual
  | Void
  | Volatile
  | WcharT
  | While
  | Xor
  | XorEq
  deriving (Eq, Ord, Show, Read)

-- | We don't have to classify tokens in detail, but it's convenient for testing and debugging.
data Token
  = -- identifier
    Ident String
  | -- literals
    Int Integer
  | Long Integer
  | LongLong Integer
  | Char Char
  | String String
  | -- punctuations
    Ampersand
  | Arrow
  | Colon
  | Comma
  | Dot
  | DoubleColon
  | Equal
  | Sharp
  | Question
  | -- parens
    OpenBracket
  | OpenParen
  | OpenBrace
  | CloseBracket
  | CloseParen
  | CloseBrace
  | -- keywords
    Keyword Keyword
  | -- operators
    Operator Operator
  | -- additional keywords
    Int8T
  | Int16T
  | Int32T
  | Int64T
  | UInt8T
  | UInt16T
  | UInt32T
  | UInt64T
  | -- REP macros
    REP
  | REP3
  | REP_R
  | REP3R
  | -- types in std::
    Std
  | Array
  | Deque
  | List
  | Map
  | Pair
  | PriorityQueue
  | Queue
  | Set
  | String'
  | Tuple
  | UnorderedMap
  | UnorderedSet
  | Vector
  | -- types in atcoder::
    AtCoder
  | Segtree
  | -- types in jikka::
    Jikka
  | ConvexHullTrick
  deriving (Eq, Ord, Show, Read)

type Token' = WithLoc Token
