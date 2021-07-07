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
    reduceSum,
    reduceProduct,
    reduceModSum,
    reduceModProduct,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules

reduceSum :: Monad m => RewriteRule m
reduceSum = simpleRewriteRule $ \case
  -- reduce list build functions
  Sum' (Nil' _) -> Just Lit0
  Sum' (Cons' _ x xs) -> Just $ Plus' x (Sum' xs)
  Sum' (Range1' n) -> Just $ FloorDiv' (Mult' n (Plus' n Lit1)) Lit2
  -- reduce list map functions
  Sum' (Reversed' _ xs) -> Just $ Sum' xs
  Sum' (Sorted' _ xs) -> Just $ Sum' xs
  Sum' (Map' t1 _ (Lam x _ e) xs) | x `isUnusedVar` e -> Just $ Mult' (Len' t1 xs) e
  Sum' (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Negate' (Sum' (Map' t1 t2 (Lam x t e) xs))
  Sum' (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) -> Just $ Plus' (Sum' (Map' t1 t2 (Lam x t e1) xs)) (Sum' (Map' t1 t2 (Lam x t e2) xs))
  Sum' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ Mult' e1 (Sum' (Map' t1 t2 (Lam x t e2) xs))
  Sum' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ Mult' e2 (Sum' (Map' t1 t2 (Lam x t e1) xs))
  -- others
  _ -> Nothing

reduceProduct :: Monad m => RewriteRule m
reduceProduct = simpleRewriteRule $ \case
  -- reduce list build functions
  Product' (Nil' _) -> Just Lit1
  Product' (Cons' _ x xs) -> Just $ Mult' x (Product' xs)
  Product' (Range1' n) -> Just $ If' IntTy (Equal' IntTy n Lit0) Lit1 Lit0
  -- reduce list map functions
  Product' (Reversed' _ xs) -> Just $ Product' xs
  Product' (Sorted' _ xs) -> Just $ Product' xs
  Product' (Map' t1 _ (Lam x _ e) xs) | x `isUnusedVar` e -> Just $ Pow' e (Len' t1 xs)
  Product' (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Mult' (Pow' (Negate' Lit0) (Len' t1 xs)) (Product' (Map' t1 t2 (Lam x t e) xs))
  Product' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) -> Just $ Mult' (Product' (Map' t1 t2 (Lam x t e1) xs)) (Product' (Map' t1 t2 (Lam x t e2) xs))
  -- others
  _ -> Nothing

reduceModSum :: Monad m => RewriteRule m
reduceModSum = simpleRewriteRule $ \case
  -- the corner case
  ModSum' _ Lit1 -> Just Lit0
  -- reduce list build functions
  ModSum' (Nil' _) _ -> Just Lit0
  ModSum' (Cons' _ x xs) m -> Just $ ModPlus' x (ModSum' xs m) m
  ModSum' (Range1' n) m -> Just $ FloorMod' (FloorDiv' (Mult' n (Plus' n Lit1)) Lit2) m
  -- reduce list map functions
  ModSum' (Reversed' _ xs) m -> Just $ ModSum' xs m
  ModSum' (Sorted' _ xs) m -> Just $ ModSum' xs m
  -- others
  _ -> Nothing

reduceModProduct :: Monad m => RewriteRule m
reduceModProduct = simpleRewriteRule $ \case
  -- the corner case
  ModProduct' _ Lit1 -> Just Lit0
  -- reduce list build functions
  ModProduct' (Nil' _) m -> Just $ FloorMod' Lit1 m
  ModProduct' (Cons' _ x xs) m -> Just $ ModMult' x (ModProduct' xs m) m
  ModProduct' (Range1' n) m -> Just $ If' IntTy (Equal' IntTy n Lit0) (FloorMod' Lit1 m) Lit0
  -- reduce list map functions
  ModProduct' (Reversed' _ xs) m -> Just $ ModProduct' xs m
  ModProduct' (Sorted' _ xs) m -> Just $ ModProduct' xs m
  -- others
  _ -> Nothing

rule :: MonadAlpha m => RewriteRule m
rule =
  mconcat
    [ reduceSum,
      reduceProduct,
      reduceModSum,
      reduceModProduct
    ]

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
