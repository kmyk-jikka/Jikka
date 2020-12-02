module Jikka.Language.Common.Name where

type Name = String

newtype VarName = VarName Name deriving (Eq, Ord, Show, Read)

unVarName :: VarName -> Name
unVarName (VarName name) = name

newtype FunName = FunName Name deriving (Eq, Ord, Show, Read)

unFunName :: FunName -> Name
unFunName (FunName name) = name

newtype TypeName = TypeName Name deriving (Eq, Ord, Show, Read)

unTypeName :: TypeName -> Name
unTypeName (TypeName name) = name
