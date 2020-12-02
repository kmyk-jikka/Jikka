{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Language.Core.Beta
-- Description : substitutes variables with another exprs.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Language.Core.Beta
  ( substitute,
    substitute',
  )
where

import Jikka.Language.Common.Name
import Jikka.Language.Core.Expr
import Jikka.Language.Core.Vars

-- | `substitute` replaces the occrences of the given variable with the given expr. This considers contexts.
--
-- >>> substitute (VarName "x") (Lit (LitInt 0)) (Lam1 (VarName "y") IntTy (Var (VarName "x")))
-- Lam [(VarName "y",IntTy)] (Lit (LitInt 0))
--
-- >>> substitute (VarName "x") (Lit (LitInt 0)) (Lam1 (VarName "x") IntTy (Var (VarName "x")))
-- Lam [(VarName "x",IntTy)] (Var (VarName "x"))
substitute :: VarName -> Expr -> Expr -> Expr
substitute = substitute' []

substitute' :: [VarName] -> VarName -> Expr -> Expr -> Expr
substitute' used x e = go
  where
    go :: Expr -> Expr
    go = \case
      Var y -> if y == x then e else Var y
      Lit lit -> Lit lit
      App f args -> App (go f) (map go args)
      Lam args body -> case () of
        _ | x `elem` map fst args -> Lam args body
        _ ->
          let rename' (y, t) (args', body') = let (y', body'') = rename (y, body') in ((y', t) : args', body'')
              (args', body') = foldr rename' ([], body) args
              used' = reverse (map fst args') ++ used
           in Lam args' (substitute' used' x e body')
      Let y t e1 e2 -> case () of
        _ | y == x -> Let y t (go e1) e2
        _ ->
          let (y', e2') = rename (y, e2)
           in Let y' t (go e1) (substitute' (y' : used) x e e2')
    rename :: (VarName, Expr) -> (VarName, Expr)
    rename (y, e')
      | not (y `isFreeVarOrScopedVar` e) = (y, e')
      | otherwise = let z = findFreshVar' ([e, e'] ++ map Var used) in (z, renameWithoutContext y z e')

renameWithoutContext :: VarName -> VarName -> Expr -> Expr
renameWithoutContext x y = go
  where
    rename :: VarName -> VarName
    rename z
      | z == x = y
      | otherwise = z
    go :: Expr -> Expr
    go = \case
      Var z -> Var (rename z)
      Lit lit -> Lit lit
      App f args -> App (go f) (map go args)
      Lam args e -> Lam (map (\(z, t) -> (rename z, t)) args) (go e)
      Let z t e1 e2 -> Let (rename z) t (go e1) (go e2)
