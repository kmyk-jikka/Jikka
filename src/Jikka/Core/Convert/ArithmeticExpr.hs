{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Jikka.Core.Convert.ArithmeticExpr
-- Description : sorts arithmetical exprs. / 算術式を整理します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.ArithmeticExpr
  ( run,
  )
where

import Jikka.Common.Error
import Jikka.Core.Language.ArithmeticExpr
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.TypeCheck
import Jikka.Core.Language.Util

runExpr :: MonadError Error m => [(VarName, Type)] -> Expr -> m Expr
runExpr env e = do
  t <- typecheckExpr env e
  if t == IntTy
    then return . formatArithmeticExpr $ parseArithmeticExpr e
    else return e

runProgram :: MonadError Error m => Program -> m Program
runProgram = mapExprProgramM (mapSubExprM runExpr) -- Doesn't use RewriteRules because the rewriting may not terminate.

-- | `run` sorts arithmetical exprs.
--
-- == Examples
--
-- Before:
--
-- > 1 + a * 1 + b - b
--
-- After:
--
-- > a + 1
run :: MonadError Error m => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.ArithmeticExpr" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
