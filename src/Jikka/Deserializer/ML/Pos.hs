{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Jikka.Deserializer.ML.Pos where

import Control.DeepSeq
import GHC.Generics (Generic)

data Pos
  = Pos
      { line :: !Int,
        column :: !Int
      }
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData Pos

data WithPos a
  = WithPos
      { pos :: !Pos,
        value :: !a
      }
  deriving (Eq, Ord, Show, Read, Generic, Functor)

instance NFData a => NFData (WithPos a)

withPos :: WithPos a -> b -> WithPos b
withPos pos value = pos {value = value}

prettyPos :: Pos -> String
prettyPos pos = "line " ++ show (line pos) ++ " column " ++ show (column pos)
