module Jikka.Language.Common.Literal where

data Literal
  = Unit
  | Bool !Bool
  | Int !Integer
  deriving (Eq, Ord, Read, Show)
