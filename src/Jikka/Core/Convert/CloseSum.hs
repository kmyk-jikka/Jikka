{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.CloseSum
-- Description : does reductions about summations and products, and tries to rewrite with closed-form exprs. / 総和と総乗についての簡約を行い、閉じた式への書き換えを目指します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.CloseSum
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
  Sum' (Reversed' _ xs) -> Just $ Sum' xs
  Product' (Reversed' _ xs) -> Just $ Product' xs
  ModSum' (Reversed' _ xs) m -> Just $ ModSum' xs m
  ModProduct' (Reversed' _ xs) m -> Just $ ModProduct' xs m
  -- reduce `Sorted`
  Sum' (Sorted' _ xs) -> Just $ Sum' xs
  Product' (Sorted' _ xs) -> Just $ Product' xs
  ModSum' (Sorted' _ xs) m -> Just $ ModSum' xs m
  ModProduct' (Sorted' _ xs) m -> Just $ ModProduct' xs m
  -- reduce `Map`
  Sum' (Map' t1 _ (Lam x _ e) xs) | x `isUnusedVar` e -> Just $ Mult' (Len' t1 xs) e
  Sum' (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Negate' (Sum' (Map' t1 t2 (Lam x t e) xs))
  Sum' (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) -> Just $ Plus' (Sum' (Map' t1 t2 (Lam x t e1) xs)) (Sum' (Map' t1 t2 (Lam x t e2) xs))
  Sum' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ Mult' e1 (Sum' (Map' t1 t2 (Lam x t e2) xs))
  Sum' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ Mult' e2 (Sum' (Map' t1 t2 (Lam x t e1) xs))
  Product' (Map' t1 _ (Lam x _ e) xs) | x `isUnusedVar` e -> Just $ Pow' e (Len' t1 xs)
  Product' (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Mult' (Pow' (Negate' Lit0) (Len' t1 xs)) (Product' (Map' t1 t2 (Lam x t e) xs))
  Product' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) -> Just $ Mult' (Product' (Map' t1 t2 (Lam x t e1) xs)) (Product' (Map' t1 t2 (Lam x t e2) xs))
  _ -> Nothing

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` reduces summations and products.
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
-- * `Sum` \(: \list(\int) \to \int\)
-- * `Product` \(: \list(\int) \to \int\)
-- * `ModSum` \(: \list(\int) \to \int \to \int\)
-- * `ModProduct` \(: \list(\int) \to \int \to \int\)
--
-- === Arithmetical functions
--
-- * `Negate` \(: \int \to \int\)
-- * `Plus` \(: \int \to \int \to \int\)
-- * `Minus` \(: \int \to \int \to \int\)
-- * `Mult` \(: \int \to \int \to \int\)
-- * `Pow` \(: \int \to \int \to \int\)
--
-- === Arithmetical functions with modulo
--
-- * `ModNegate` \(: \int \to \int \to \int\)
-- * `ModPlus` \(: \int \to \int \to \int \to \int\)
-- * `ModMinus` \(: \int \to \int \to \int \to \int\)
-- * `ModMult` \(: \int \to \int \to \int \to \int\)
-- * `ModPow` \(: \int \to \int \to \int \to \int\)
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
run prog = wrapError' "Jikka.Core.Convert.CloseSum" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
