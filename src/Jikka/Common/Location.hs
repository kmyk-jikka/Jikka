{-# LANGUAGE DeriveFunctor #-}

module Jikka.Common.Location where

-- | `Loc` represents a location of something in the users' source code. `line` and `column` is 1-based.
data Loc = Loc
  { line :: !Int,
    column :: !Int,
    width :: !Int
  }
  deriving (Eq, Ord, Show, Read)

data WithLoc a = WithLoc
  { loc :: !Loc,
    value :: !a
  }
  deriving (Eq, Ord, Show, Read, Functor)
