{-# LANGUAGE DeriveFunctor #-}

module Jikka.Common.Location where

import Data.String (IsString (..))

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

data WithLoc' a = WithLoc'
  { loc' :: !(Maybe Loc),
    value' :: !a
  }
  deriving (Eq, Ord, Show, Read, Functor)

instance IsString a => IsString (WithLoc' a) where
  fromString = WithLoc' Nothing . fromString

withoutLoc :: a -> WithLoc' a
withoutLoc = WithLoc' Nothing
