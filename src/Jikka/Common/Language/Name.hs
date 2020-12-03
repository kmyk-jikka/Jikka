{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jikka.Common.Language.Name where

import Data.String (IsString)

type Name = String

newtype VarName = VarName Name deriving (Eq, Ord, Show, Read, IsString)

unVarName :: VarName -> Name
unVarName (VarName name) = name

newtype FunName = FunName Name deriving (Eq, Ord, Show, Read, IsString)

unFunName :: FunName -> Name
unFunName (FunName name) = name

newtype TypeName = TypeName Name deriving (Eq, Ord, Show, Read, IsString)

unTypeName :: TypeName -> Name
unTypeName (TypeName name) = name
