module Jikka.CPlusPlus.Parse.Util where

import Jikka.CPlusPlus.Language.Expr

data TypeSpecifier
  = NameTySpec Bool [String] TypeName
  | CharTySpec
  | Char16TySpec
  | Char32TySpec
  | WcharTySpec
  | BoolTySpec
  | ShortTySpec
  | IntTySpec
  | LongTySpec
  | SignedTySpec
  | UnsignedTySpec
  | FloatTySpec
  | DoubleTySpec
  | VoidTySpec
  | AutoTySpec
  deriving (Eq, Ord, Show, Read)

data CVQualifier
  = ConstQualifier
  | VolatileQualifier
  deriving (Eq, Ord, Show, Read)

data DeclSpecifier
  = StorageSepcDeclSpec StorageClassSpecifier
  | TypeSpecDeclSpec TypeSpecifier
  | FuncSpecDeclSpec FunctionSpecifier
  | FriendDeclSpec
  | TypedefDeclSpec
  | ConstExperDeclSpec
  deriving (Eq, Ord, Show, Read)

data StorageClassSpecifier
  = RegisterStorageSpec
  | StaticStorageSpec
  | ThreadLocalStorageSpec
  | ExternStorageSpec
  | MutableStorageSpec
  deriving (Eq, Ord, Show, Read)

data FunctionSpecifier
  = InlineFuncSpec
  | VirtualFuncSpec
  | ExplicitFuncSpec
  deriving (Eq, Ord, Show, Read)
