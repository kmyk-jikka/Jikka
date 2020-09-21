module Jikka.Language.Common.BuiltIn where

data BuiltIn
  = Not
  | And
  | Or
  | If
  | Succ
  | Pred
  | Neg
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Min
  | Max
  | Sum
  | Product
  | Minimum
  | Maximum
  | Equal
  | NotEqual
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  deriving (Eq, Ord, Enum, Bounded, Read, Show)
