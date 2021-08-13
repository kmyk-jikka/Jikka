{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.RemoveUnusedVars
-- Description : removes unused variables. / 使われていない変数を削除します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.RemoveUnusedVars` remove unused variables from exprs.
module Jikka.Core.Convert.RemoveUnusedVars
  ( run,
    run',
  )
where

import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars (isUnusedVar)
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Util

runExpr :: [(VarName, Type)] -> Expr -> Expr
runExpr _ = mapSubExpr go []
  where
    go _ = \case
      Let x _ _ e2 | x `isUnusedVar` e2 -> e2
      e -> e

-- | TODO: Remove `ToplevelLet` if its variable is not used.
runToplevelExpr :: [(VarName, Type)] -> ToplevelExpr -> ToplevelExpr
runToplevelExpr _ = \case
  ToplevelLetRec f args ret body cont ->
    if isUnusedVar f body
      then ToplevelLet f (curryFunTy (map snd args) ret) (curryLam args body) cont
      else ToplevelLetRec f args ret body cont
  e -> e

run' :: Program -> Program
run' = mapToplevelExprProgram runToplevelExpr . mapExprProgram (mapSubExpr runExpr)

-- | `run` removes unused variables in given programs.
--
-- This also removes variables for recursion, i.e. "rec" flags.
-- `ToplevelLetRec` may becomes `ToplevelLet`.
--
-- For example, this converts
--
-- > let rec solve x =
-- >     let y = 0
-- >     in x
-- > in solve
--
-- to
--
-- > let solve x =
-- >     x
-- > in solve
run :: MonadError Error m => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.RemoveUnusedVars" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- return $ run' prog
  postcondition $ do
    ensureWellTyped prog
  return prog
