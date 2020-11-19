module Jikka.Language.Common.Name where

type Name = String

newtype VarName = VarName {unVarName :: Name} deriving (Eq, Ord, Show, Read)

newtype FunName = FunName {unFunName :: Name} deriving (Eq, Ord, Show, Read)
