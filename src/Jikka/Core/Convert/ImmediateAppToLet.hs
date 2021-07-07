{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.ImmediateAppToLet
-- Description : replaces immediate applications to lambda abstractions with let-exprs. / lambda 抽象の直後に行なわれる関数適用を let 式で置き換えます。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.ImmediateAppToLet
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.Alpha as Alpha
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Util

runExpr :: [(VarName, Type)] -> Expr -> Expr
runExpr _ = \case
  App (Lam x t body) e -> Let x t e body
  e -> e

runProgram :: Program -> Program
runProgram = mapExprProgram runExpr

-- | `run` does beta-reductions in given programs.
-- For example, this converts the following:
--
-- > (fun x -> x + x) a
--
-- to the follwoing:
--
-- > let x = a in x + x
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.ImmediateAppToLet" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- Alpha.run prog
  prog <- return $ runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
