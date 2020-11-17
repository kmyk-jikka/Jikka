{-# LANGUAGE DeriveFunctor #-}

module Jikka.Deserializer.Pos where

data Pos
  = Pos
      { line :: !Int,
        column :: !Int
      }
  deriving (Eq, Ord, Show, Read)

data WithPos a
  = WithPos
      { pos :: !Pos,
        value :: !a
      }
  deriving (Eq, Ord, Show, Read, Functor)

withPos :: WithPos a -> b -> WithPos b
withPos pos value = pos {value = value}

prettyPos :: Pos -> String
prettyPos pos = "line " ++ show (line pos) ++ " column " ++ show (column pos)
