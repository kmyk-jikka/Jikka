{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.Beta
-- Description : does beta-reductions. / beta 簡約を行います。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.Beta
  ( run,

    -- * internal rules
    rule,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.Alpha as Alpha
import Jikka.Core.Language.Beta
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules

rule :: MonadAlpha m => RewriteRule m
rule = RewriteRule $ \_ -> \case
  App (Lam x _ e1) e2 -> Just <$> substitute x e2 e1
  _ -> return Nothing

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` does beta-reduction.
--
-- == Examples
--
-- Before:
--
-- > (fun x -> x + x) y
--
-- After:
--
-- > y + y
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.Beta" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- Alpha.run prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
