{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.CloseAll
-- Description : does reductions about @all@ and @any@, and tries to rewrite with closed-form exprs. / @all@ と @any@ についての簡約を行い、閉じた式への書き換えを目指します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.CloseAll
  ( run,

    -- * internal rules
    rule,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules

rule :: Monad m => RewriteRule m
rule = simpleRewriteRule $ \case
  -- reduce `Reversed`
  All' (Reversed' _ xs) -> Just $ All' xs
  Any' (Reversed' _ xs) -> Just $ Any' xs
  -- reduce `Sorted`
  All' (Sorted' _ xs) -> Just $ All' xs
  Any' (Sorted' _ xs) -> Just $ Any' xs
  -- reduce `Map`
  _ -> Nothing

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` reduces `All` and `Any`.
--
-- == List of builtin functions which are reduced
--
-- \[
--     \newcommand\int{\mathbf{int}}
--     \newcommand\bool{\mathbf{bool}}
--     \newcommand\list{\mathbf{list}}
-- \]
--
-- === Target functions
--
-- * `All` \(: \list(\bool) \to \bool\)
-- * `Any` \(: \list(\bool) \to \bool\)
--
-- === Boolean functions
--
-- * `Not` \(: \bool \to \bool\)
-- * `And` \(: \bool \to \bool \to \bool\)
-- * `Or` \(: \bool \to \bool \to \bool\)
-- * `Implies` \(: \bool \to \bool \to \bool\)
--
-- === List Build functions
--
-- * `Nil` \(: \forall \alpha. \list(\alpha)\)
-- * `Cons` \(: \forall \alpha. \alpha \to \list(\alpha) \to \list(\alpha)\)
-- * `Range1` \(: \int \to \list(\int)\)
-- * `Tabulate` \(: \forall \alpha. \int \to (\int \to \alpha) \to \list(\alpha)\)
--
-- === List Map functions
--
-- * `Scanl` \(: \forall \alpha \beta. (\beta \to \alpha \to \beta) \to \beta \to \list(\alpha) \to \list(\beta)\)
-- * `Map` \(: \forall \alpha \beta. (\alpha \to \beta) \to \list(\alpha) \to \list(\beta)\)
-- * `Filter` \(: \forall \alpha \beta. (\alpha \to \bool) \to \list(\alpha) \to \list(\beta)\)
-- * `Reversed` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
-- * `Sorted` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.CloseAll" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
