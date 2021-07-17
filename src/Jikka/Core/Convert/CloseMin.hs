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
    reduceMin,
    reduceMax,
    reduceArgMin,
    reduceArgMax,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules

reduceMin :: Monad m => RewriteRule m
reduceMin = simpleRewriteRule $ \case
  -- list build functions
  Min1' t (Nil' _) -> Just $ Bottom' t "no minimum in empty list"
  Min1' _ (Cons' _ e (Nil' _)) -> Just e
  Min1' t (Cons' _ e (Cons' _ e' es)) -> Just $ Min2' t e (Min1' t (Cons' t e' es))
  -- list map functions
  Min1' t (Reversed' _ es) -> Just $ Min1' t es
  Min1' t (Cons' _ e (Reversed' _ es)) -> Just $ Min1' t (Cons' t e es)
  Min1' t (Sorted' _ es) -> Just $ Min1' t es
  Min1' t (Cons' _ e (Sorted' _ es)) -> Just $ Min1' t (Cons' t e es)
  Min1' t (Map' t1 t2 f es) -> case f of
    Lam x _ e | x `isUnusedVar` e -> Just e
    Lam x _ (Min2' _ e1 e2) -> Just $ Min2' t (Min1' t (Map' t1 t2 (Lam x t e1) es)) (Min1' t (Map' t1 t2 (Lam x t e2) es))
    Lam x _ (Negate' e) -> Just $ Negate' (Max1' t (Map' t1 t2 (Lam x IntTy e) es))
    Lam x _ (Plus' e1 e2) | x `isUnusedVar` e1 -> Just $ Plus' e1 (Min1' t (Map' t1 t2 (Lam x IntTy e2) es))
    Lam x _ (Plus' e1 e2) | x `isUnusedVar` e2 -> Just $ Plus' (Min1' t (Map' t1 t2 (Lam x IntTy e1) es)) e1
    _ -> Nothing
  Min1' t (Cons' _ e0 (Map' t1 t2 f xs)) -> case f of
    Lam x _ e | x `isUnusedVar` e -> Just $ If' t (Equal' IntTy (Len' t xs) Lit0) e0 (Min2' t e0 e)
    Lam x _ (Min2' _ e1 e2) -> Just $ Min2' t (Min1' t (Cons' t e0 (Map' t1 t2 (Lam x t e1) xs))) (Min1' t (Cons' t e0 (Map' t1 t2 (Lam x t e2) xs)))
    Lam x _ (Negate' e) -> Just $ Negate' (Max1' t (Cons' t (Negate' e0) (Map' t1 t2 (Lam x IntTy e) xs)))
    Lam x _ (Plus' e1 e2) | x `isUnusedVar` e1 -> Just $ Plus' e1 (Min1' t (Cons' t (Minus' e0 e1) (Map' t1 t2 (Lam x IntTy e2) xs)))
    Lam x _ (Plus' e1 e2) | x `isUnusedVar` e2 -> Just $ Plus' (Min1' t (Cons' t (Minus' e0 e1) (Map' t1 t2 (Lam x IntTy e1) xs))) e1
    _ -> Nothing
  _ -> Nothing

reduceMax :: Monad m => RewriteRule m
reduceMax = simpleRewriteRule $ \case
  -- list build functions
  Max1' t (Nil' _) -> Just $ Bottom' t "no maximum in empty list"
  Max1' _ (Cons' _ e (Nil' _)) -> Just e
  Max1' t (Cons' _ e (Cons' _ e' es)) -> Just $ Max2' t e (Max1' t (Cons' t e' es))
  -- list map functions
  Max1' t (Reversed' _ es) -> Just $ Max1' t es
  Max1' t (Cons' _ e (Reversed' _ es)) -> Just $ Max1' t (Cons' t e es)
  Max1' t (Sorted' _ es) -> Just $ Max1' t es
  Max1' t (Cons' _ e (Sorted' _ es)) -> Just $ Max1' t (Cons' t e es)
  Max1' t (Map' t1 t2 f es) -> case f of
    Lam x _ e | x `isUnusedVar` e -> Just e
    Lam x _ (Max2' _ e1 e2) -> Just $ Max2' t (Map' t1 t2 (Lam x t e1) es) (Map' t1 t2 (Lam x t e2) es)
    Lam x _ (Negate' e) -> Just $ Negate' (Min1' t2 (Map' t1 t2 (Lam x IntTy e) es))
    Lam x _ (Plus' e1 e2) | x `isUnusedVar` e1 -> Just $ Plus' e1 (Max1' t2 (Map' t1 t2 (Lam x IntTy e2) es))
    Lam x _ (Plus' e1 e2) | x `isUnusedVar` e2 -> Just $ Plus' (Max1' t2 (Map' t1 t2 (Lam x IntTy e1) es)) e1
    _ -> Nothing
  Max1' t (Cons' _ e0 (Map' t1 t2 f xs)) -> case f of
    Lam x _ e | x `isUnusedVar` e -> Just $ If' t (Equal' IntTy (Len' t xs) Lit0) e0 (Max2' t e0 e)
    Lam x _ (Max2' _ e1 e2) -> Just $ Max2' t (Max1' t (Cons' t e0 (Map' t1 t2 (Lam x t e1) xs))) (Max1' t (Cons' t e0 (Map' t1 t2 (Lam x t e2) xs)))
    Lam x _ (Negate' e) -> Just $ Negate' (Min1' t (Cons' t (Negate' e0) (Map' t1 t2 (Lam x IntTy e) xs)))
    Lam x _ (Plus' e1 e2) | x `isUnusedVar` e1 -> Just $ Plus' e1 (Max1' t (Cons' t (Minus' e0 e1) (Map' t1 t2 (Lam x IntTy e2) xs)))
    Lam x _ (Plus' e1 e2) | x `isUnusedVar` e2 -> Just $ Plus' (Max1' t (Cons' t (Minus' e0 e1) (Map' t1 t2 (Lam x IntTy e1) xs))) e1
    _ -> Nothing
  _ -> Nothing

-- | TODO: implement this
reduceArgMin :: Monad m => RewriteRule m
reduceArgMin = simpleRewriteRule $ \case
  -- list map functions
  ArgMin' t (Reversed' _ xs) -> Just $ Minus' (Minus' (Len' t xs) (ArgMin' t xs)) Lit1
  ArgMin' _ (Map' _ _ (Lam x _ e) _) | x `isUnusedVar` e -> Just Lit0
  ArgMin' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ ArgMin' t2 (Map' t1 t2 (Lam x t e2) xs)
  ArgMin' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ ArgMin' t2 (Map' t1 t2 (Lam x t e1) xs)
  _ -> Nothing

-- | TODO: implement this
reduceArgMax :: Monad m => RewriteRule m
reduceArgMax = simpleRewriteRule $ \case
  -- list map functions
  ArgMax' t (Reversed' _ xs) -> Just $ Minus' (Minus' (Len' t xs) (ArgMax' t xs)) Lit1
  ArgMax' _ (Map' _ _ (Lam x t e) xs) | x `isUnusedVar` e -> Just $ Minus' (Len' t xs) Lit1
  ArgMax' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ ArgMax' t2 (Map' t1 t2 (Lam x t e2) xs)
  ArgMax' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ ArgMax' t2 (Map' t1 t2 (Lam x t e1) xs)
  _ -> Nothing

rule :: Monad m => RewriteRule m
rule =
  mconcat
    [ reduceMin,
      reduceMax,
      reduceArgMin,
      reduceArgMax
    ]

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` reduces maximums and minimums.
--
-- == Examples
--
-- Before:
--
-- > max (map (fun x -> 3 + f x) xs)
--
-- After:
--
-- > 3 + max (map f xs)
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
