{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Language.Util where

import Data.Char (isAlphaNum)
import Jikka.CPlusPlus.Language.Expr
import Jikka.Common.Alpha

fromLeftExpr :: LeftExpr -> Expr
fromLeftExpr = \case
  LeftVar x -> Var x
  LeftAt x e -> At (fromLeftExpr x) e
  LeftGet n e -> Call (Function "std::get" [TyIntValue n]) [fromLeftExpr e]

data NameKind
  = LocalNameKind
  | LocalArgumentNameKind
  | ConstantNameKind
  | FunctionNameKind
  | ArgumentNameKind
  deriving (Eq, Ord, Show, Read)

fromNameKind :: NameKind -> String
fromNameKind = \case
  LocalNameKind -> "x"
  LocalArgumentNameKind -> "b"
  ConstantNameKind -> "c"
  FunctionNameKind -> "f"
  ArgumentNameKind -> "a"

newFreshName :: MonadAlpha m => NameKind -> m VarName
newFreshName kind = renameVarName kind ""

renameVarName :: MonadAlpha m => NameKind -> String -> m VarName
renameVarName kind hint = do
  i <- nextCounter
  let prefix = case takeWhile (\c -> isAlphaNum c || c == '_') hint of
        "" -> fromNameKind kind
        hint' -> hint' ++ "_"
  return (VarName (prefix ++ show i))
