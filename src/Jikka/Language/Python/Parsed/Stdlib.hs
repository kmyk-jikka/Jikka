module Jikka.Language.Python.Parsed.Stdlib where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Jikka.Language.Common.Name
import Jikka.Language.Python.Typed.Stdlib

unaryOps :: M.Map FunName UnaryOp
unaryOps =
  let op name unop = (FunName name, unop)
   in M.fromList
        [ op "abs" Abs,
          op "all" All,
          op "any" Any,
          op "argmax" ArgMax,
          op "argmin" ArgMin,
          op "~" BitNot,
          op "fact" Fact,
          op "max" Max1,
          op "min" Min1,
          op "-" Negate,
          op "not" Not,
          op "product" Product,
          op "range" Range1,
          op "sum" Sum
        ]

genericUnaryOps :: M.Map FunName (ChurchType -> UnaryOp)
genericUnaryOps = M.fromList
    [ (FunName "len", Len),
      (FunName "list", List),
      (FunName "reversed", Reversed),
      (FunName "sorted", Sorted)
    ]

binaryOps :: M.Map FunName BinaryOp
binaryOps =
  let op name binop = (FunName name, binop)
   in M.fromList
        [ op "and" And,
          op "&" BitAnd,
          op "<<" BitLeftShift,
          op "|" BitOr,
          op ">>" BitRightShift,
          op "^" BitXor,
          op "/^" CeilDiv,
          op "ceildiv" CeilDiv,
          op "choose" Choose,
          op "//" FloorDiv,
          op "floordiv" FloorDiv,
          op "%" FloorMod,
          op "gcd" Gcd,
          op ">=" GreaterEqual,
          op ">" GreaterThan,
          op "implies" Implies,
          op "inv" Inv,
          op "lcm" Lcm,
          op "<=" LessEqual,
          op "<" LessThan,
          op ">?" Max,
          op "max" Max,
          op "<?" Min,
          op "min" Min,
          op "-" Minus,
          op "*" Mult,
          op "multiChoose" MultiChoose,
          op "or" Or,
          op "permute" Permute,
          op "+" Plus,
          op "**" Pow,
          op "pow" Pow,
          op "range" Range2
        ]

genericBinaryOps :: M.Map FunName (ChurchType -> BinaryOp)
genericBinaryOps = M.fromList
    [ (FunName "==", Equal),
      (FunName "!=", NotEqual)
    ]

ternaryOps :: M.Map FunName TernaryOp
ternaryOps = M.fromList
    [ (FunName "pow", PowMod),
      (FunName "range", Range3)
    ]

operatorNames :: S.Set FunName
operatorNames =
  S.unions
    [ M.keysSet genericUnaryOps,
      M.keysSet genericBinaryOps,
      M.keysSet unaryOps,
      M.keysSet binaryOps,
      M.keysSet ternaryOps
    ]
