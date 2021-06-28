{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Language.Util where

import Jikka.CPlusPlus.Language.Expr

fromLeftExpr :: LeftExpr -> Expr
fromLeftExpr = \case
  LeftVar x -> Var x
  LeftAt x e -> At (fromLeftExpr x) e
  LeftGet n e -> Call (Function "std::get" [TyIntValue n]) [fromLeftExpr e]
