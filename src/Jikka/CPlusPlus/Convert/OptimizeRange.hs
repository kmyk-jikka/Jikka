{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.CPlusPlus.Convert.OptimizeRange
-- Description : reduces about @range@ function. / @range@ 関数について簡約します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.CPlusPlus.Convert.OptimizeRange
  ( run,
  )
where

import Jikka.CPlusPlus.Language.Expr
import Jikka.CPlusPlus.Language.Util
import Jikka.Common.Error

runExpr :: Monad m => Expr -> m Expr
runExpr = \case
  Call' At [Call' Range [_], i] -> return i
  Call' MethodSize [Call' Range [n]] -> return n
  e -> return e

runStatement :: Monad m => Statement -> m Statement
runStatement = \case
  ForEach _ x (Call' Range [n]) body -> return $ repStatement x n body -- TODO: check n is not updated in body
  stmt -> return stmt

runProgram :: Monad m => Program -> m Program
runProgram = mapExprStatementProgramM runExpr (((: []) <$>) . runStatement)

-- | `run` replaces superfluous copying.
--
-- == Examples
--
-- Before:
--
-- > int b = range(a).size();
--
-- After:
--
-- > int b = a;
--
-- Before:
--
-- > for (int i : jikka::range(n)) {
-- >     ...
-- > }
--
-- After:
--
-- > for (int i = 0; i < n; ++ i) {
-- >     ...
-- > }
run :: MonadError Error m => Program -> m Program
run prog = wrapError' "Jikka.CPlusPlus.Convert.OptimizeRange" $ do
  runProgram prog
