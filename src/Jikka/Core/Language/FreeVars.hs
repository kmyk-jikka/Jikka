{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Converter.Core.FreeVars
-- Description : provides utilities aboud free variables. / 自由変数についてのユーティリティを提供します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Language.FreeVars where

import Data.Maybe
import qualified Data.Set as S
import Jikka.Core.Language.Expr

freeVars :: Expr -> S.Set VarName
freeVars = \case
  Var x -> S.singleton x
  Lit _ -> S.empty
  App f e -> freeVars f <> freeVars e
  Lam x _ e -> S.delete x (freeVars e)
  Let x _ e1 e2 -> freeVars e1 <> S.delete x (freeVars e2)
  Assert e1 e2 -> freeVars e1 <> freeVars e2

-- | `isFreeVar` checks if the given variable occurs in the tiven expr. This considers contexts.
--
-- >>> VarName "x" `isFreeVar` Lam (VarName "y") IntTy (Var (VarName "x"))
-- True
--
-- >>> VarName "x" `isFreeVar` Lam (VarName "x") IntTy (Var (VarName "x"))
-- False
isFreeVar :: VarName -> Expr -> Bool
isFreeVar x = \case
  Var y -> y == x
  Lit _ -> False
  App f e -> isFreeVar x f || isFreeVar x e
  Lam y _ e -> x /= y && isFreeVar x e
  Let y _ e1 e2 -> (y /= x && isFreeVar x e1) || isFreeVar x e2
  Assert e1 e2 -> isFreeVar x e1 || isFreeVar x e2

-- | `isUnusedVar` is the negation of `isFreeVar`.
--
-- TODO: rename to `isNonFreeVar`?
isUnusedVar :: VarName -> Expr -> Bool
isUnusedVar x e = not (isFreeVar x e)

-- | `isFreeVarOrScopedVar` checks if the given variable occurs in the tiven expr. This ignores contexts.
--
-- >>> VarName "x" `isFreeVarOrScopedVar` Lam (VarName "x") IntTy (Var (VarName "y"))
-- True
isFreeVarOrScopedVar :: VarName -> Expr -> Bool
isFreeVarOrScopedVar x = \case
  Var y -> y == x
  Lit _ -> False
  App f e -> isFreeVarOrScopedVar x f || isFreeVarOrScopedVar x e
  Lam y _ e -> x == y || isFreeVarOrScopedVar x e
  Let y _ e1 e2 -> y == x || isFreeVarOrScopedVar x e1 || isFreeVarOrScopedVar x e2
  Assert e1 e2 -> isFreeVarOrScopedVar x e1 || isFreeVarOrScopedVar x e2

freeTyVars :: Type -> [TypeName]
freeTyVars = \case
  VarTy x -> [x]
  IntTy -> []
  BoolTy -> []
  ListTy t -> freeTyVars t
  TupleTy ts -> concatMap freeTyVars ts
  FunTy t1 t2 -> freeTyVars t1 ++ freeTyVars t2
  DataStructureTy _ -> []

findUnusedVarName :: VarName -> Expr -> VarName
findUnusedVarName (VarName x _) e =
  let xs = S.fromList (concatMap (\(VarName _ i) -> maybeToList i) (S.toList (freeVars e)))
      flavour = head $ filter (`S.notMember` xs) [0 ..]
   in VarName x (Just flavour)

findUnusedVarName' :: Expr -> VarName
findUnusedVarName' = findUnusedVarName (VarName Nothing Nothing)
