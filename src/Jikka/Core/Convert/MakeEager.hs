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

runExpr :: Expr -> Expr
runExpr = \case
  Var x -> Var x
  Lit lit -> Lit lit
  App f args -> case (runExpr f, args) of
    (Builtin (If t), [p, a, b]) -> App (AppBuiltin (If (FunTy [] t)) [runExpr p, Lam [] (runExpr a), Lam [] (runExpr b)]) []
    (f, _) -> App f (map runExpr args)
  Lam args e -> Lam args (runExpr e)
  Let x t e1 e2 -> Let x t (runExpr e1) (runExpr e2)

runToplevelExpr :: ToplevelExpr -> ToplevelExpr
runToplevelExpr e = case e of
  ResultExpr e -> ResultExpr $ runExpr e
  ToplevelLet x t e cont -> ToplevelLet x t (runExpr e) (runToplevelExpr cont)
  ToplevelLetRec x args ret body cont -> ToplevelLetRec x args ret (runExpr body) (runToplevelExpr cont)

run' :: Program -> Program
run' = runToplevelExpr

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
