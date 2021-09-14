{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.CPlusPlus.Convert.InlineSetAt
-- Description : does inline expansion of @set_at@ function. / @set_at@ 関数を inline 展開します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.CPlusPlus.Convert.InlineSetAt
  ( run,
  )
where

import Control.Monad.Writer.Strict
import Jikka.CPlusPlus.Language.Expr
import Jikka.CPlusPlus.Language.Util
import Jikka.Common.Alpha
import Jikka.Common.Error

runExpr :: (MonadAlpha m, MonadWriter [Statement] m) => Expr -> m Expr
runExpr = \case
  Call' (SetAt t) [xs, i, x] -> do
    y <- case xs of
      Var xs -> renameVarName LocalNameHint xs
      _ -> newFreshName LocalNameHint
    tell
      [ Declare (TyVector t) y (DeclareCopy xs),
        Assign (AssignExpr SimpleAssign (LeftAt (LeftVar y) i) x)
      ]
    return (Var y)
  e -> return e

runStatement :: MonadAlpha m => Statement -> m [Statement]
runStatement stmt = do
  (stmt, decls) <- runWriterT (mapDirectExprStatementM (mapSubExprM runExpr) stmt)
  return $ decls ++ [stmt]

runProgram :: MonadAlpha m => Program -> m Program
runProgram = mapExprStatementProgramM return runStatement

-- | `run` does inline expansion of @jikka::set_at<T>(...)@ function.
--
-- == Examples
--
-- Before:
--
-- > func(jikka::set_at<T>(xs, i, x));
--
-- After:
--
-- > vector<int> ys = xs;
-- > ys[i] = x;
-- > func(ys);
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.CPlusPlus.Convert.InlineSetAt" $ do
  runProgram prog
