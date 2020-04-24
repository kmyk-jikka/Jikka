module Jikka.Optimizer.Type.Type where

data Type
  = Unit
  | Bool
  | Int
  | Fun Type Type
  deriving (Eq, Ord, Read, Show)
