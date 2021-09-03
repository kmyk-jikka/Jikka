{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Jikka.Core.Convert.CloseSum
-- Description : does reductions about summations and products, and tries to rewrite with closed-form exprs. / 総和と総乗についての簡約を行い、閉じた式への書き換えを目指します。
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
import qualified Jikka.Core.Convert.Alpha as Alpha
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.LambdaPatterns
import Jikka.Core.Language.Lint
import Jikka.Core.Language.QuasiRules
import Jikka.Core.Language.RewriteRules

reduceSum :: MonadAlpha m => RewriteRule m
reduceSum =
  mconcat
    [ -- reduce list build functions
      [r| "sum/nil" sum nil = 0 |],
      [r| "sum/cons" forall x xs. sum (cons x xs) = x + sum xs |],
      [r| "sum/range" forall n. sum (range n) = n * (n - 1) /! 2 |],
      -- reduce list map functions
      [r| "sum/reversed" forall xs. sum (reversed xs) = sum xs |],
      [r| "sum/sorted" forall xs. sum (sorted xs) = sum xs |],
      [r| "sum/filter/map" forall g f xs. sum (filter g (map f xs)) = sum (map (fun x -> if g (f x) then f x else 0) xs) |],
      makeRewriteRule "sum/map/const" $ \_ -> \case
        Sum' (Map' _ _ (LamConst t e) xs) -> return . Just $ Mult' (Len' t xs) e
        _ -> return Nothing,
      [r| "sum/map/id" forall n. sum (map (fun x -> x) (range n)) = n * (n - 1) /! 2 |],
      [r| "sum/map/pow/2" forall n. sum (map (fun x -> x ** 2) (range n)) = n * (n - 1) * (2 * n - 1) /! 6 |],
      [r| "sum/map/pow/2'" forall n. sum (map (fun x -> x * x) (range n)) = n * (n - 1) * (2 * n - 1) /! 6 |],
      [r| "sum/map/negate" forall e xs. sum (map (fun x -> - e) xs) = - sum (map (fun x -> e) xs) |],
      [r| "sum/map/plus" forall e1 e2 xs. sum (map (fun x -> e1 + e2) xs) = sum (map (fun x -> e1) xs) + sum (map (fun x -> e2) xs) |],
      [r| "sum/map/minus" forall e1 e2 xs. sum (map (fun x -> e1 - e2) xs) = sum (map (fun x -> e1) xs) - sum (map (fun x -> e2) xs) |],
      makeRewriteRule "sum/map/mult" $ \_ -> \case
        Sum' (Map' t1 t2 (Lam x t (Mult' k e)) xs) | x `isUnusedVar` k -> return . Just $ Mult' k (Sum' (Map' t1 t2 (Lam x t e) xs))
        _ -> return Nothing,
      makeRewriteRule "sum/map/mult'" $ \_ -> \case
        Sum' (Map' t1 t2 (Lam x t (Mult' e k)) xs) | x `isUnusedVar` k -> return . Just $ Mult' k (Sum' (Map' t1 t2 (Lam x t e) xs))
        _ -> return Nothing
    ]

-- | TODO: implement this.
reduceProduct :: Monad m => RewriteRule m
reduceProduct = simpleRewriteRule "reduceProduct" $ \case
  Product' xs -> case xs of
    -- reduce list build functions
    Nil' _ -> Just Lit1
    Cons' _ x xs -> Just $ Mult' x (Product' xs)
    Range1' n -> Just $ If' IntTy (Equal' IntTy n Lit0) Lit1 Lit0
    -- reduce list map functions
    Reversed' _ xs -> Just $ Product' xs
    Sorted' _ xs -> Just $ Product' xs
    Map' t1 _ (Lam x _ e) xs | x `isUnusedVar` e -> Just $ Pow' e (Len' t1 xs)
    Map' t1 t2 (Lam x t (Negate' e)) xs -> Just $ Mult' (Pow' (Negate' Lit0) (Len' t1 xs)) (Product' (Map' t1 t2 (Lam x t e) xs))
    Map' t1 t2 (Lam x t (Mult' e1 e2)) xs -> Just $ Mult' (Product' (Map' t1 t2 (Lam x t e1) xs)) (Product' (Map' t1 t2 (Lam x t e2) xs))
    -- others
    _ -> Nothing
  _ -> Nothing

-- |
-- * This assumes that `ModFloor` is already propagated.
reduceModSum :: MonadAlpha m => RewriteRule m
reduceModSum =
  mconcat
    [ -- the corner case
      [r| "modsum/1" forall xs. modsum xs 1 = 0 |],
      -- reduce list build functions
      [r| "modsum/nil" forall m. modsum nil m = 0 |],
      [r| "modsum/cons" forall m x xs. modsum (cons x xs) m = modplus x (sum xs) m |],
      [r| "modsum/range" forall m n. modsum (range n) m = (n * (n - 1) /! 2) % m |],
      -- reduce list map functions
      [r| "modsum/reversed" forall m xs. modsum (reversed xs) m = modsum xs m |],
      [r| "modsum/sorted" forall m xs. modsum (sorted xs) m = modsum xs m |],
      [r| "modsum/filter/map" forall m g f xs. modsum (filter g (map f xs)) m = modsum (map (fun x -> if g (f x) then f x else 0) xs) m |],
      makeRewriteRule "modsum/map/const" $ \_ -> \case
        ModSum' (Map' _ _ (LamConst t e) xs) m -> return . Just $ ModMult' (Len' t xs) e m
        _ -> return Nothing,
      makeRewriteRule "modsum/map/floormod/const" $ \_ -> \case
        ModSum' (Map' _ _ (LamConst t (FloorMod' e m')) xs) m | m' == m -> return . Just $ ModMult' (Len' t xs) e m
        _ -> return Nothing,
      [r| "modsum/map/id" forall m n. modsum (map (fun x -> x) (range n)) m = n * (n - 1) /! 2 % m |],
      [r| "modsum/map/floormod/id" forall m n. modsum (map (fun x -> x % m) (range n)) m = n * (n - 1) /! 2 % m |],
      [r| "modsum/map/modpow/2" forall m n. modsum (map (fun x -> modpow x 2 m) (range n)) m = n * (n - 1) * (2 * n - 1) /! 6 % m |],
      [r| "modsum/map/modpow/2'" forall m n. modsum (map (fun x -> modmult x x m) (range n)) m = n * (n - 1) * (2 * n - 1) /! 6 % m |],
      [r| "modsum/map/modnegate" forall m e xs. modsum (map (fun x -> modnegate e m) xs) m = modnegate (modsum (map (fun x -> e) xs) m) m |],
      [r| "modsum/map/modplus" forall m e1 e2 xs. modsum (map (fun x -> modplus e1 e2 m) xs) m = modplus (modsum (map (fun x -> e1) xs) m) (modsum (map (fun x -> e2) xs) m) m |],
      [r| "modsum/map/modminus" forall m e1 e2 xs. modsum (map (fun x -> modminus e1 e2 m) xs) m = modminus (modsum (map (fun x -> e1) xs) m) (modsum (map (fun x -> e2) xs) m) m |],
      makeRewriteRule "modsum/map/modmult" $ \_ -> \case
        ModSum' (Map' t1 t2 (Lam x t (ModMult' k e m')) xs) m | x `isUnusedVar` k && m' == m -> return . Just $ ModMult' k (ModSum' (Map' t1 t2 (Lam x t e) xs) m) m
        _ -> return Nothing,
      makeRewriteRule "modsum/map/modmult'" $ \_ -> \case
        ModSum' (Map' t1 t2 (Lam x t (ModMult' e k m')) xs) m | x `isUnusedVar` k && m' == m -> return . Just $ ModMult' k (ModSum' (Map' t1 t2 (Lam x t e) xs) m) m
        _ -> return Nothing
    ]

-- | TODO: implement this.
reduceModProduct :: Monad m => RewriteRule m
reduceModProduct = simpleRewriteRule "reduceModProduct" $ \case
  ModProduct' xs m -> case xs of
    -- the corner case
    _ | m == Lit1 -> Just Lit0
    -- reduce list build functions
    Nil' _ -> Just $ FloorMod' Lit1 m
    Cons' _ x xs -> Just $ ModMult' x (ModProduct' xs m) m
    Range1' n -> Just $ If' IntTy (Equal' IntTy n Lit0) (FloorMod' Lit1 m) Lit0
    -- reduce list map functions
    Reversed' _ xs -> Just $ ModProduct' xs m
    Sorted' _ xs -> Just $ ModProduct' xs m
    -- others
    _ -> Nothing
  _ -> Nothing

rule :: MonadAlpha m => RewriteRule m
rule =
  mconcat
    [ reduceSum,
      reduceProduct,
      reduceModSum,
      reduceModProduct
    ]

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` reduces summations and products.
--
-- * This doen't do nothing about `Foldl`.
--
-- == Examples
--
-- Before:
--
-- > foldl (fun y x -> y + x) 0 (range n)
--
-- After:
--
-- > x * (x - 1) / 2
--
-- == List of builtin functions which are reduced
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
--
-- === List Map functions
--
-- * `Map` \(: \forall \alpha \beta. (\alpha \to \beta) \to \list(\alpha) \to \list(\beta)\)
-- * `Filter` \(: \forall \alpha \beta. (\alpha \to \bool) \to \list(\alpha) \to \list(\beta)\)
-- * `Reversed` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
-- * `Sorted` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.CloseSum" $ do
  precondition $ do
    lint prog
  prog <- runProgram prog
  prog <- Alpha.run prog
  postcondition $ do
    lint prog
  return prog
