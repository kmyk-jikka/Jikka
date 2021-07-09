{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.CloseMin
-- Description : does reductions about minnimums and maximums of lists, and tries to rewrite with closed-form exprs. / リストの最小値と最大値についての簡約を行い、閉じた式への書き換えを目指します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.CloseMin
  ( run,

    -- * internal rules
    rule,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules

rule :: Monad m => RewriteRule m
rule = simpleRewriteRule $ \case
  -- reduce `Reversed`
  Max1' t (Reversed' _ xs) -> Just $ Max1' t xs
  Min1' t (Reversed' _ xs) -> Just $ Min1' t xs
  ArgMax' t (Reversed' _ xs) -> Just $ Minus' (Minus' (Len' t xs) (ArgMax' t xs)) Lit1
  ArgMin' t (Reversed' _ xs) -> Just $ Minus' (Minus' (Len' t xs) (ArgMin' t xs)) Lit1
  -- reduce `Sorted`
  Max1' t (Sorted' _ xs) -> Just $ Max1' t xs
  Min1' t (Sorted' _ xs) -> Just $ Min1' t xs
  -- reduce `Map`
  Max1' _ (Map' _ _ (Lam x _ e) _) | x `isUnusedVar` e -> Just e
  Max1' _ (Map' t1 t2 (Lam x t (Max2' t' e1 e2)) xs) -> Just $ Max2' t' (Map' t1 t2 (Lam x t e1) xs) (Map' t1 t2 (Lam x t e2) xs)
  Max1' _ (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Negate' (Min1' t2 (Map' t1 t2 (Lam x t e) xs))
  Max1' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ Plus' e1 (Max1' t2 (Map' t1 t2 (Lam x t e2) xs))
  Max1' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ Plus' (Max1' t2 (Map' t1 t2 (Lam x t e1) xs)) e1
  Min1' _ (Map' _ _ (Lam x _ e) _) | x `isUnusedVar` e -> Just e
  Min1' _ (Map' t1 t2 (Lam x t (Min2' t' e1 e2)) xs) -> Just $ Min2' t' (Map' t1 t2 (Lam x t e1) xs) (Map' t1 t2 (Lam x t e2) xs)
  Min1' _ (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Negate' (Max1' t2 (Map' t1 t2 (Lam x t e) xs))
  Min1' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ Plus' e1 (Min1' t2 (Map' t1 t2 (Lam x t e2) xs))
  Min1' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ Plus' (Min1' t2 (Map' t1 t2 (Lam x t e1) xs)) e1
  ArgMax' _ (Map' _ _ (Lam x t e) xs) | x `isUnusedVar` e -> Just $ Minus' (Len' t xs) Lit1
  ArgMax' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ ArgMax' t2 (Map' t1 t2 (Lam x t e2) xs)
  ArgMax' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ ArgMax' t2 (Map' t1 t2 (Lam x t e1) xs)
  ArgMin' _ (Map' _ _ (Lam x _ e) _) | x `isUnusedVar` e -> Just Lit0
  ArgMin' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ ArgMin' t2 (Map' t1 t2 (Lam x t e2) xs)
  ArgMin' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ ArgMin' t2 (Map' t1 t2 (Lam x t e1) xs)
  _ -> Nothing

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` reduces maximums and minimums.
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
-- * `Max1` \(: \forall \alpha. \list(\alpha) \to \alpha\)
-- * `Min1` \(: \forall \alpha. \list(\alpha) \to \alpha\)
-- * `ArgMax` \(: \forall \alpha. \list(\alpha) \to \int\)
-- * `ArgMin` \(: \forall \alpha. \list(\alpha) \to \int\)
--
-- === Related functions
--
-- * `Max2` \(: \forall \alpha. \alpha \to \alpha \to \alpha\)
-- * `Min2` \(: \forall \alpha. \alpha \to \alpha \to \alpha\)
--
-- === List Build functions
--
-- * `Nil` \(: \forall \alpha. \list(\alpha)\)
-- * `Cons` \(: \forall \alpha. \alpha \to \list(\alpha) \to \list(\alpha)\)
-- * `Range1` \(: \int \to \list(\int)\)
--
-- === List Map functions
--
-- * `Scanl` \(: \forall \alpha \beta. (\beta \to \alpha \to \beta) \to \beta \to \list(\alpha) \to \list(\beta)\)
-- * `Map` \(: \forall \alpha \beta. (\alpha \to \beta) \to \list(\alpha) \to \list(\beta)\)
-- * `Filter` \(: \forall \alpha \beta. (\alpha \to \bool) \to \list(\alpha) \to \list(\beta)\)
-- * `Reversed` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
-- * `Sorted` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.CloseMin" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
