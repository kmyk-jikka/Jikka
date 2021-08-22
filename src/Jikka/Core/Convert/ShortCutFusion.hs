{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Jikka.Core.Convert.ShortCutFusion
-- Description : does short cut fusion. / short cut fusion を行います。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- \[
--     \newcommand\int{\mathbf{int}}
--     \newcommand\bool{\mathbf{bool}}
--     \newcommand\list{\mathbf{list}}
-- \]
module Jikka.Core.Convert.ShortCutFusion
  ( run,

    -- * internal rules
    rule,
    reduceBuild,
    reduceMapBuild,
    reduceMap,
    reduceMapMap,
    reduceFoldMap,
    reduceFold,
    reduceFoldBuild,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Format (formatExpr)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.QuasiRules
import Jikka.Core.Language.RewriteRules

-- |
-- * `Range1` remains.
-- * `Range2` is removed.
-- * `Range3` is removed.
-- * `Nil` and `Cons` are kept as is.
reduceBuild :: MonadAlpha m => RewriteRule m
reduceBuild =
  mconcat
    [ [r| "range2" forall l r. range2 l r = map (fun i -> l + i) (range (r - l)) |],
      [r| "range3" forall l r step. range3 l r step = map (fun i -> l + i * step) (range ((r - l) /^ step)) |]
    ]

reduceMapBuild :: MonadAlpha m => RewriteRule m
reduceMapBuild =
  mconcat
    [ [r| "sorted/nil" sorted nil = nil |],
      [r| "sorted/range" forall n. sorted (range n) = range n |],
      [r| "reversed/nil" reversed nil = nil |],
      [r| "reversed/range" forall n. reversed (range n) = map (fun i -> n - i - 1) (range n) |],
      [r| "filter/nil" filter _ nil = nil |],
      [r| "map/nil" map _ nil = nil |],
      [r| "map/cons" forall f x xs. map f (cons x xs) = cons (f x) (map f xs) |]
    ]

reduceMap :: Monad m => RewriteRule m
reduceMap =
  mconcat
    [ [r| "map/id" forall xs. map (fun x -> x) xs = xs |],
      [r| "filter/const-false" forall xs. filter (fun _ -> false) xs = nil |],
      [r| "filter/const-true" forall xs. filter (fun _ -> true) xs = xs |]
    ]

-- |
-- * Functions are reordered as:
--   * `Sort` and `Reversed` (functions to reorder) are lastly applied to lists
--   * `Map` (functions to modify lists)
--   * `Filter` (funcitons to reduce lengths) is firstly applied to lists
reduceMapMap :: MonadAlpha m => RewriteRule m
reduceMapMap =
  mconcat
    [ [r| "map/map" forall f g xs. map g (map f xs) = map (fun x -> g (f x)) xs |],
      [r| "map/reversed" forall f xs. map f (reversed xs) = reversed (map f xs) |],
      [r| "filter/filter" forall f g xs. filter g (filter f xs) = filter (fun x -> f x && g x) xs |],
      [r| "filter/sorted" forall f xs. filter f (sorted xs) = sorted (filter f xs) |],
      [r| "filter/reversed" forall f xs. filter f (reversed xs) = reversed (filter f xs) |],
      [r| "reversed/reversed" forall xs. reversed (reversed xs) = xs |],
      [r| "sorted/reversed" forall xs. sorted (reversed xs) = sorted xs |],
      [r| "sorted/sorted" forall xs. sorted (sorted xs) = sorted xs |]
    ]

reduceFoldMap :: MonadAlpha m => RewriteRule m
reduceFoldMap =
  mconcat
    [ -- reduce `Reversed`
      [r| "len/reversed" forall xs. len (reversed xs) = len xs |],
      [r| "elem/reversed" forall x xs. elem x (reversed xs) = elem x xs |],
      [r| "at/reversed" forall xs i. (reversed xs)[i] = xs[len(xs) - i - 1] |],
      -- reduce `Sorted`
      [r| "len/sorted" forall xs. len (sorted xs) = len xs |],
      [r| "elem/sorted" forall x xs. elem x (sorted xs) = elem x xs |],
      -- reduce `Map`
      [r| "len/map" forall f xs. len (map f xs) = len xs |],
      [r| "at/map" forall f xs i. (map f xs)[i] = f xs[i] |],
      [r| "foldl/map" forall g init f xs. foldl g init (map f xs) = foldl (fun y x -> g y (f x)) init xs|],
      -- others
      [r| "len/setat" forall xs i x. len xs[i <- x] = len xs |],
      [r| "len/scanl" forall f init xs. len (scanl f init xs) = len xs + 1 |],
      [r| "at/setat" forall xs i x j. xs[i <- x][j] = if i == j then x else xs[j] |]
    ]

reduceFold :: Monad m => RewriteRule m
reduceFold = simpleRewriteRule "foldl->iterate" $ \case
  Foldl' t1 t2 (Lam2 x2 _ x1 _ body) init xs | x1 `isUnusedVar` body -> Just $ Iterate' t2 (Len' t1 xs) (Lam x2 t2 body) init
  _ -> Nothing

reduceFoldBuild :: MonadAlpha m => RewriteRule m
reduceFoldBuild =
  mconcat
    [ -- reduce `Foldl`
      [r| "foldl/nil" forall f init. foldl f init nil = init |],
      [r| "foldl/cons" forall f init x xs. foldl f init (cons x xs) = foldl f (f init x) xs |],
      -- reduce `Len`
      [r| "len/nil" len nil = 0 |],
      [r| "len/cons" forall x xs. len (cons x xs) = 1 + len xs |],
      [r| "len/range" forall n. len (range n) = n |],
      -- reduce `At`
      simpleRewriteRule "at/nil" $ \case
        At' t (Nil' _) i -> Just $ Bottom' t $ "cannot subscript empty list: index = " ++ formatExpr i
        _ -> Nothing,
      [r| "at/cons" forall x xs i. (cons x xs)[i] = if i == 0 then x else xs[i - 1] |],
      [r| "at/range" forall n i. (range n)[i] = i |],
      -- reduce `Elem`
      [r| "elem/nil" forall y. elem y nil = false |],
      [r| "elem/cons" forall y x xs. elem y (cons x xs) = y == x || elem y xs |],
      [r| "elem/range" forall i n. elem i (range n) = 0 <= i && i < n |],
      -- others
      [r| "len/build" forall f base n. len (build f base n) = len base + n |]
    ]

rule :: MonadAlpha m => RewriteRule m
rule =
  mconcat
    [ reduceFoldMap,
      reduceMap,
      reduceMapMap,
      reduceFoldBuild,
      reduceMapBuild,
      reduceBuild,
      reduceFold
    ]

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` does short cut fusion.
--
-- * This function is mainly for polymorphic reductions. This dosn't do much about concrete things, e.g., arithmetical operations.
-- * This does nothing about `Build`, `Scanl` or `SetAt` except combinations with `Len` or `At`.
--
-- == Example
--
-- Before:
--
-- > length (map f (cons (-1) (range n)))
--
-- After:
--
-- > n + 1
--
-- == List of builtin functions which are reduced
--
-- === Build functions
--
-- * `Nil` \(: \forall \alpha. \list(\alpha)\)
-- * `Cons` \(: \forall \alpha. \alpha \to \list(\alpha) \to \list(\alpha)\)
-- * `Range1` \(: \int \to \list(\int)\)
-- * `Range2` \(: \int \to \int \to \list(\int)\)
-- * `Range3` \(: \int \to \int \to \int \to \list(\int)\)
--
-- === Map functions
--
-- * `Map` \(: \forall \alpha \beta. (\alpha \to \beta) \to \list(\alpha) \to \list(\beta)\)
-- * `Filter` \(: \forall \alpha \beta. (\alpha \to \bool) \to \list(\alpha) \to \list(\beta)\)
-- * `Reversed` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
-- * `Sorted` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
--
-- === Fold functions
--
-- * `Foldl` \(: \forall \alpha \beta. (\beta \to \alpha \to \beta) \to \beta \to \list(\alpha) \to \beta\)
-- * `Len` \(: \forall \alpha. \list(\alpha) \to \int\)
-- * `At` \(: \forall \alpha. \list(\alpha) \to \int \to \alpha\)
-- * `Elem` \(: \forall \alpha. \alpha \to \list(\alpha) \to \bool\)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.ShortCutFusion" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
