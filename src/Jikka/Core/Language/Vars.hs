{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Converter.Core.Vars
-- Description : provides utilities for variables.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Language.Vars where

import Jikka.Core.Language.Expr

-- | `isFreeVar` checks if the given variable occurs in the tiven expr. This considers contexts.
--
-- >>> VarName "x" `isFreeVar` Lam1 (VarName "y") IntTy (Var (VarName "x"))
-- True
--
-- >>> VarName "x" `isFreeVar` Lam1 (VarName "x") IntTy (Var (VarName "x"))
-- False
isFreeVar :: VarName -> Expr -> Bool
isFreeVar x = \case
  Var y -> y == x
  Lit _ -> False
  App f args -> isFreeVar x f || any (isFreeVar x) args
  Lam args e -> x `notElem` map fst args && isFreeVar x e
  Let y _ e1 e2 -> (y /= x && isFreeVar x e1) || isFreeVar x e2

-- | `isUnusedVar` is the negation of `isFreeVar`.
--
-- TODO: rename to `isNonFreeVar`?
isUnusedVar :: VarName -> Expr -> Bool
isUnusedVar x e = not (isFreeVar x e)

-- | `isFreeVarOrScopedVar` checks if the given variable occurs in the tiven expr. This ignores contexts.
--
-- >>> VarName "x" `isFreeVarOrScopedVar` Lam1 (VarName "x") IntTy (Var (VarName "y"))
-- True
isFreeVarOrScopedVar :: VarName -> Expr -> Bool
isFreeVarOrScopedVar x = \case
  Var y -> y == x
  Lit _ -> False
  App f args -> isFreeVarOrScopedVar x f || any (isFreeVarOrScopedVar x) args
  Lam args e -> x `elem` map fst args || isFreeVarOrScopedVar x e
  Let y _ e1 e2 -> y == x || isFreeVarOrScopedVar x e1 || isFreeVarOrScopedVar x e2

-- | `findFreshVar` generates a variable which isn't free in the given expr. The return variable name may conflict if you ignores the context.
findFreshVar :: Expr -> VarName
findFreshVar e = findFreshVar' [e]

-- | TODO: make a function to list free/scoped vars and use it to optimize the time complexity.
findFreshVar' :: [Expr] -> VarName
findFreshVar' es = head . filter pred $ map getAnonymousVar [0 ..]
  where
    pred x = all (\e -> not (x `isFreeVarOrScopedVar` e)) es

getAnonymousVar :: Int -> VarName
getAnonymousVar i = VarName ("@" ++ show i)

freeTyVars :: Type -> [TypeName]
freeTyVars = \case
  VarTy x -> [x]
  IntTy -> []
  BoolTy -> []
  ListTy t -> freeTyVars t
  TupleTy ts -> concatMap freeTyVars ts
  FunTy ts ret -> concatMap freeTyVars (ret : ts)
