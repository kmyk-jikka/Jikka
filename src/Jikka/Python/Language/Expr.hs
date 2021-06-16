{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Jikka.Python.Language.Expr
-- Description : contains data types of the standard Python.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Python.Language.Expr` module has the basic data types for the standard Python.
-- See the Python's @ast@ module (<https://docs.python.org/ja/3/library/ast.html#abstract-grammar>) for reference.
module Jikka.Python.Language.Expr where

import Data.Int (Int8)
import Data.String (IsString)
import Jikka.Common.Location

newtype Ident = Ident String deriving (Eq, Ord, Show, Read, IsString)

unIdent :: Ident -> String
unIdent (Ident x) = x

type Ident' = WithLoc Ident

data Constant
  = ConstNone
  | ConstInt Integer
  | ConstBool Bool
  | ConstString String
  | ConstBytes [Int8]
  | ConstFloat Double
  | ConstImaginary Double
  deriving (Eq, Ord, Show, Read)

data Statement
  = FunctionDef Ident' Arguments [Statement'] [Decorator] (Maybe Type')
  | AsyncFunctionDef Ident' Arguments [Statement'] [Decorator] (Maybe Type')
  | ClassDef Ident' [Expr'] [Keyword'] [Statement'] [Decorator]
  | Return (Maybe Expr')
  | Delete [Target']
  | Assign [Target'] Expr'
  | AugAssign Target' Operator Expr'
  | AnnAssign Target' Type' (Maybe Expr')
  | -- | @For target iter body orelse@
    For Target' Expr' [Statement'] [Statement']
  | AsyncFor Target' Expr' [Statement'] [Statement']
  | -- | @While test body orelse@
    While Expr' [Statement'] [Statement']
  | If Expr' [Statement'] [Statement']
  | With [WithItem] [Statement']
  | AsyncWith [WithItem] [Statement']
  | -- | @Raise exc cause@ represents @raise exc from cause@.
    Raise (Maybe Expr') (Maybe Expr')
  | -- | @Try body handlers orelse finalbody@
    Try [Statement'] [ExceptHandler'] [Statement'] [Statement']
  | -- | @Assert test msg@
    Assert Expr' (Maybe Expr')
  | Import [Alias]
  | ImportFrom [Ident'] [Alias]
  | Global [Ident']
  | Nonlocal [Ident']
  | Expr' Expr'
  | Pass
  | Break
  | Continue
  deriving (Eq, Ord, Show, Read)

type Statement' = WithLoc Statement

data Expr
  = BoolOp Expr' BoolOp Expr'
  | -- | produced by the walrus operator @:=@
    NamedExpr Target' Expr'
  | BinOp Expr' Operator Expr'
  | UnaryOp UnaryOp Expr'
  | Lambda Arguments Expr'
  | -- | @IfExp test body orelse@
    IfExp Expr' Expr' Expr'
  | -- | NULL key is for @**d@.
    Dict [(Maybe Expr', Expr')]
  | Set [Expr']
  | ListComp Expr' [Comprehension]
  | SetComp Expr' [Comprehension]
  | DictComp Expr' Expr' [Comprehension]
  | GeneratorExp Expr' [Comprehension]
  | Await Expr'
  | Yield (Maybe Expr')
  | YieldFrom Expr'
  | Compare Expr' [(CmpOp, Expr')]
  | Call Expr' [Expr'] [Keyword']
  | -- | @FormattedValue value conversion format_spec@ for f-strings
    FormattedValue Expr' (Maybe Char) (Maybe Expr')
  | JoinedStr [Expr'] -- for f-strings
  | Constant Constant
  | -- | can appear in assignment context
    Attribute Expr' Ident'
  | -- | can appear in assignment context
    Subscript Expr' Expr'
  | -- | can appear in assignment context
    Starred Expr'
  | -- | can appear in assignment context
    Name Ident'
  | -- | can appear in assignment context
    List [Expr']
  | -- | can appear in assignment context
    Tuple [Expr']
  | -- | @Slice lower upper step@ can appear only in Subscript
    Slice (Maybe Expr') (Maybe Expr') (Maybe Expr')
  deriving (Eq, Ord, Show, Read)

type Expr' = WithLoc Expr

type Target = Expr'

type Target' = Expr'

type Type = Expr'

type Type' = Expr'

type Decorator = Expr'

type Decorator' = Expr'

data ExprContext = Load | Store | Del
  deriving (Eq, Ord, Show, Read)

data BoolOp
  = And
  | Or
  | -- | our extension
    Implies
  deriving (Eq, Ord, Show, Read)

data Operator
  = Add
  | Sub
  | Mult
  | MatMult
  | Div
  | FloorDiv
  | FloorMod
  | -- | our extension
    CeilDiv
  | -- | our extension
    CeilMod
  | Pow
  | BitLShift
  | BitRShift
  | BitOr
  | BitXor
  | BitAnd
  | -- | our extension
    Max
  | -- | our extension
    Min
  deriving (Eq, Ord, Show, Read)

data UnaryOp
  -- | on int
  = Invert
  -- | on bool
  | Not
  | UAdd
  | USub
  deriving (Eq, Ord, Show, Read)

data CmpOp
  = Eq'
  | NotEq
  | Lt
  | LtE
  | Gt
  | GtE
  | Is
  | IsNot
  | In
  | NotIn
  deriving (Eq, Ord, Show, Read)

-- | @Comprehension target iter ifs is_async@
data Comprehension
  = Comprehension
      { compTarget :: Target',
        compIter :: Expr',
        compIfs :: Maybe Expr'
      }
  deriving (Eq, Ord, Show, Read)

data ExceptHandler
  = ExceptHandler
      { exchType :: Maybe Type',
        exchName :: Maybe Ident',
        exchBody :: [Statement']
      }
  deriving (Eq, Ord, Show, Read)

type ExceptHandler' = WithLoc ExceptHandler

data Arguments
  = Arguments
      { argsPosonlyargs :: [Arg],
        argsArgs :: [Arg],
        argsVarargs :: Maybe Arg,
        argsKwonlyargs :: [Arg],
        argsKwDefaults :: [Expr'],
        argsKwarg :: Maybe Arg,
        argsDefaults :: [Expr']
      }
  deriving (Eq, Ord, Show, Read)

emptyArguments :: Arguments
emptyArguments =
  Arguments
    { argsPosonlyargs = [],
      argsArgs = [],
      argsVarargs = Nothing,
      argsKwonlyargs = [],
      argsKwDefaults = [],
      argsKwarg = Nothing,
      argsDefaults = []
    }

type Arg = (Ident', Maybe Type')

-- | NULL identifier is for @**kwargs@.
type Keyword = (Maybe Ident', Expr')

type Keyword' = WithLoc Keyword

-- | @(name, asname)@. `Alias` is used for `Import` and `ImportFrom`.
type Alias = (Ident', Maybe Ident')

-- | @(context_expr, optional_vars)@
type WithItem = (Expr', Maybe Expr')

type Program = [Statement']
