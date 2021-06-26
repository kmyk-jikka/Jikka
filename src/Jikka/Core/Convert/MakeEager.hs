{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.MakeEager
-- Description : convert exprs for eager evaluation.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.MakeEager
  ( run,
    run',
  )
where

import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Util

runExpr :: [(VarName, Type)] -> Expr -> Expr
runExpr _ = \case
  App f args -> case (f, args) of
    (Builtin (If t), [p, a, b]) -> App (AppBuiltin (If (FunTy [] t)) [p, Lam [] a, Lam [] b]) []
    (f, _) -> App f args
  e -> e

run' :: Program -> Program
run' = mapExprProgram runExpr

-- | `run` wraps some exprs with lambda redundant things from AST.
-- Specifically, this converts @if p then a else b@ to something like @(if p then (lambda. a) else (lambda. b))()@.
--
-- For example, this converts:
--
-- > let rec fact n =
-- >     if n == 0 then
-- >         1
-- >     else
-- >         n * fact (n - 1)
-- > in fact 10
--
-- to:
--
-- > let rec fact n =
-- >     (if n == 0 then
-- >         fun -> 1
-- >     else
-- >         fun -> n * fact (n - 1)
-- >     )()
-- > in fact 10
run :: MonadError Error m => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.MakeEager" $ do
  prog <- return $ run' prog
  ensureWellTyped prog
  return prog
