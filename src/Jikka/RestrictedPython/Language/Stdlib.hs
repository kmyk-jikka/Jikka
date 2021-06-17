{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Language.Stdlib where

import qualified Data.Set as S
import Jikka.RestrictedPython.Language.Expr

builtinFunctions :: S.Set Ident
builtinFunctions = S.union standardBuiltinFunctions additionalBuiltinFunctions

standardBuiltinFunctions :: S.Set Ident
standardBuiltinFunctions =
  S.fromList
    [ "abs",
      "delattr",
      "hash",
      "memoryview",
      "set",
      "all",
      "dict",
      "help",
      "min",
      "setattr",
      "any",
      "dir",
      "hex",
      "next",
      "slice",
      "ascii",
      "divmod",
      "id",
      "object",
      "sorted",
      "bin",
      "enumerate",
      "input",
      "oct",
      "staticmethod",
      "bool",
      "eval",
      "int",
      "open",
      "str",
      "breakpoint",
      "exec",
      "isinstance",
      "ord",
      "sum",
      "bytearray",
      "filter",
      "issubclass",
      "pow",
      "super",
      "bytes",
      "float",
      "iter",
      "print",
      "tuple",
      "callable",
      "format",
      "len",
      "property",
      "type",
      "chr",
      "frozenset",
      "list",
      "range",
      "vars",
      "classmethod",
      "getattr",
      "locals",
      "repr",
      "zip",
      "compile",
      "globals",
      "map",
      "reversed",
      "__import__",
      "complex",
      "hasattr",
      "max",
      "round"
    ]

additionalBuiltinFunctions :: S.Set Ident
additionalBuiltinFunctions =
  S.fromList
    [ "argmax",
      "argmin",
      "ceildiv",
      "ceilmod",
      "choose",
      "fact",
      "floordiv",
      "floormod",
      "gcd",
      "inv",
      "lcm",
      "multichoose",
      "permute",
      "product"
    ]
