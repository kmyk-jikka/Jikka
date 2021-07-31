{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Jikka.Core.Language.LambdaPatterns where

import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars

pattern LamId t <-
  (\case Lam x t (Var y) | x == y -> Just t; _ -> Nothing -> Just t)
  where
    LamId t = Lam "x" t (Var "x")

pattern LamConst t e <-
  (\case Lam x t e | x `isUnusedVar` e -> Just (t, e); _ -> Nothing -> Just (t, e))
  where
    LamConst t e = Lam (findUnusedVarName' e) t e
