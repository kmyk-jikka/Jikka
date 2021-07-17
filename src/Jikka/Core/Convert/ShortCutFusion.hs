{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

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
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

-- |
-- * `Range1` remains.
-- * `Range2` is removed.
-- * `Range3` is removed.
-- * `Nil` and `Cons` are kept as is.
reduceBuild :: MonadAlpha m => RewriteRule m
reduceBuild =
  let return' = return . Just
   in RewriteRule $ \_ -> \case
        Range2' l r -> do
          let n = Minus' r l
          x <- genVarName'
          let f = Lam x IntTy (Plus' l (Var x))
          return' $ Map' IntTy IntTy f (Range1' n)
        Range3' l r step -> do
          let n = CeilDiv' (Minus' r l) step
          x <- genVarName'
          let f = Lam x IntTy (Plus' l (Mult' step (Var x)))
          return' $ Map' IntTy IntTy f (Range1' n)
        _ -> return Nothing

reduceMapBuild :: MonadAlpha m => RewriteRule m
reduceMapBuild =
  let return' = return . Just
   in RewriteRule $ \_ -> \case
        -- reduce `Sorted`
        Sorted' _ (Nil' t) -> return' $ Nil' t
        Sorted' _ (Range1' n) -> return' $ Range1' n
        -- reduce `Reversed`
        Reversed' _ (Nil' t) -> return' $ Nil' t
        Reversed' _ (Range1' n) -> do
          x <- genVarName'
          let f = Lam x IntTy (Minus' (Minus' n (Var x)) (LitInt' 1))
          return' $ Map' IntTy IntTy f n
        -- reduce `Filter`
        Filter' _ _ (Nil' t) -> return' $ Nil' t
        -- reduce `Map`
        Map' _ _ _ (Nil' t) -> return' $ Nil' t
        Map' t1 t2 f (Cons' _ x xs) -> return' $ Cons' t2 (App f x) (Map' t1 t2 f xs)
        -- others
        _ -> return Nothing

reduceMap :: Monad m => RewriteRule m
reduceMap =
  let return' = return . Just
   in RewriteRule $ \_ -> \case
        -- reduce `Map`
        Map' _ _ (LamId _ _) xs -> return' xs
        -- reduce `Filter`
        Filter' t (Lam _ _ LitFalse) _ -> return' (Nil' t)
        Filter' _ (Lam _ _ LitTrue) xs -> return' xs
        -- others
        _ -> return Nothing

-- |
-- * Functions are reordered as:
--   * `Sort` and `Reversed` (functions to reorder) are lastly applied to lists
--   * `Map` (functions to modify lists)
--   * `Filter` (funcitons to reduce lengths) is firstly applied to lists
reduceMapMap :: MonadAlpha m => RewriteRule m
reduceMapMap =
  let return' = return . Just
   in RewriteRule $ \_ -> \case
        -- reduce `Map`
        Map' _ _ (LamId _ _) xs -> return' xs
        Map' _ t3 g (Map' t1 _ f xs) -> do
          x <- genVarName'
          let h = Lam x t1 (App g (App f (Var x)))
          return' $ Map' t1 t3 h xs
        Map' t1 t2 f (Reversed' _ xs) -> return' $ Reversed' t2 (Map' t1 t2 f xs)
        -- reduce `Filter`
        Filter' t2 g (Map' t1 _ f xs) -> do
          x <- genVarName'
          let h = Lam x t1 (App g (App f (Var x)))
          return' $ Map' t1 t2 f (Filter' t1 h xs)
        Filter' t g (Filter' _ f xs) -> do
          x <- genVarName'
          let h = Lam x t (And' (App g (Var x)) (App f (Var x)))
          return' $ Filter' t h xs
        Filter' t f (Sorted' _ xs) -> return' $ Sorted' t (Filter' t f xs)
        Filter' t f (Reversed' _ xs) -> return' $ Reversed' t (Filter' t f xs)
        -- reduce `Reversed`
        Reversed' _ (Reversed' _ xs) -> return' xs
        Reversed' _ (Map' t1 t2 f xs) -> return' $ Map' t1 t2 f (Reversed' t1 xs)
        -- reduce `Sorted`
        Sorted' t (Reversed' _ xs) -> return' $ Sorted' t xs
        Sorted' t (Sorted' _ xs) -> return' $ Sorted' t xs
        -- others
        _ -> return Nothing

reduceFoldMap :: MonadAlpha m => RewriteRule m
reduceFoldMap =
  let return' = return . Just
   in RewriteRule $ \_ -> \case
        -- reduce `Reversed`
        Len' t (Reversed' _ xs) -> return' $ Len' t xs
        Elem' t x (Reversed' _ xs) -> return' $ Elem' t x xs
        At' t (Reversed' _ xs) i -> return' $ At' t xs (Minus' (Minus' (Len' t xs) i) Lit1)
        -- reduce `Sorted`
        Len' t (Sorted' _ xs) -> return' $ Len' t xs
        Elem' t x (Sorted' _ xs) -> return' $ Elem' t x xs
        -- reduce `Map`
        Len' _ (Map' t1 _ _ xs) -> return' $ Len' t1 xs
        At' _ (Map' t1 _ f xs) i -> return' $ App f (At' t1 xs i)
        Foldl' _ t3 g init (Map' t1 _ f xs) -> do
          x3 <- genVarName'
          x1 <- genVarName'
          return' $ Foldl' t1 t3 (Lam2 x3 t3 x1 t1 (App2 g (Var x3) (App f (Var x1)))) init xs
        -- others
        _ -> return Nothing

reduceFold :: Monad m => RewriteRule m
reduceFold = simpleRewriteRule $ \case
  Foldl' t1 t2 (Lam2 x2 _ x1 _ body) init xs | x1 `isUnusedVar` body -> Just $ Iterate' t2 (Len' t1 xs) (Lam x2 t2 body) init
  _ -> Nothing

reduceFoldBuild :: MonadAlpha m => RewriteRule m
reduceFoldBuild =
  let return' = return . Just
   in RewriteRule $ \_ -> \case
        -- reduce `Foldl`
        Foldl' _ _ _ init (Nil' _) -> return' init
        Foldl' t1 t2 g init (Cons' _ x xs) -> return' $ Foldl' t1 t2 g (App2 g init x) xs
        -- reduce `Len`
        Len' _ (Nil' _) -> return' Lit0
        Len' t (Cons' _ _ xs) -> return' $ Plus' Lit1 (Len' t xs)
        Len' _ (Range1' n) -> return' n
        -- reduce `At`
        At' t (Nil' _) i -> return' $ Bottom' t $ "cannot subscript empty list: index = " ++ formatExpr i
        At' t (Cons' _ x xs) i -> return' $ If' t (Equal' IntTy i Lit0) x (At' t xs (Minus' i Lit1))
        At' _ (Range1' _) i -> return' i
        -- reduce `Elem`
        Elem' _ _ (Nil' _) -> return' LitFalse
        Elem' t y (Cons' _ x xs) -> return' $ And' (Equal' t x y) (Elem' t y xs)
        Elem' _ x (Range1' n) -> return' $ And' (LessEqual' IntTy Lit0 x) (LessThan' IntTy x n)
        -- others
        _ -> return Nothing

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

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` does short cut fusion.
--
-- * This function is mainly for polymorphic reductions. This dosn't do much about concrete things, e.g., arithmetical operations.
-- * This doesn't do nothing about `Scanl` or `SetAt`.
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
