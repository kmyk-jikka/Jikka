{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Converter.Core.Vars
-- Description : provides utilities for variables.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Language.Core.Vars where

import Jikka.Language.Common.Name
import Jikka.Language.Core.Expr

isFreeVar :: VarName -> Expr -> Bool
isFreeVar x = \case
  Var y -> y == x
  Lit _ -> False
  App f args -> isFreeVar x f || any (isFreeVar x) args
  Lam args e -> x `notElem` map fst args && isFreeVar x e
  Let y _ e1 e2 -> (y /= x && isFreeVar x e1) || isFreeVar x e2

isUnusedVar :: VarName -> Expr -> Bool
isUnusedVar x e = not (isFreeVar x e)

-- | `findFreshVar` generates a variable which isn't free in the given expr. The return variable name may conflict if you ignores the context.
findFreshVar :: Expr -> VarName
findFreshVar e = findFreshVar' [e]

findFreshVar' :: [Expr] -> VarName
findFreshVar' es = head . filter pred $ map getAnonymousVar [0 ..]
  where
    pred x = all (x `isUnusedVar`) es

getAnonymousVar :: Int -> VarName
getAnonymousVar i = VarName ("a@" ++ show i)
