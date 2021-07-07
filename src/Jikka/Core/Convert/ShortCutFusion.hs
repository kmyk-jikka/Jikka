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
module Jikka.Core.Convert.ShortCutFusion
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

-- | `reduceBuild` converts all other list constucting functions to `Range1`.
reduceBuild :: MonadAlpha m => RewriteRule m
reduceBuild = RewriteRule $ \_ -> \case
  Tabulate' t n f -> return . Just $ Map' IntTy t f (Range1' n)
  Range2' l r -> do
    let n = Minus' r l
    x <- genVarName'
    return . Just $ Tabulate' IntTy (Lam x IntTy (Plus' l (Var x))) n
  Range3' l r step -> do
    let n = CeilDiv' (Minus' r l) step
    x <- genVarName'
    return . Just $ Tabulate' IntTy (Lam x IntTy (Plus' l (Mult' step (Var x)))) n
  _ -> return Nothing

reduceMapMap :: MonadAlpha m => RewriteRule m
reduceMapMap = RewriteRule $ \_ -> \case
  -- reduce `Reversed`
  Reversed' _ (Reversed' _ xs) -> return $ Just xs
  Reversed' _ (Map' t1 t2 f xs) -> return . Just $ Map' t1 t2 f (Reversed' t1 xs)
  -- reduce `Sorted`
  Sorted' t (Reversed' _ xs) -> return . Just $ Sorted' t xs
  Sorted' t (Sorted' _ xs) -> return . Just $ Sorted' t xs
  Sorted' _ (Range1' n) -> return . Just $ Range1' n
  Sorted' _ (Map' t1 t2 f xs) -> return . Just $ Map' t1 t2 f (Sorted' t1 xs)
  -- reduce `Map`
  Map' _ _ (LamId _ _) xs -> return $ Just xs
  Map' _ t3 f (Map' t1 _ (Lam x t e) xs) -> return . Just $ Map' t1 t3 (Lam x t (App f e)) xs
  Map' _ t3 g (Map' t1 _ f xs) -> do
    x <- genVarName'
    return . Just $ Map' t1 t3 (Lam x t1 (App g (App f (Var x)))) xs
  _ -> return Nothing

reduceFoldMap :: Monad m => RewriteRule m
reduceFoldMap = simpleRewriteRule $ \case
  -- reduce `Reversed`
  Len' t (Reversed' _ xs) -> Just $ Len' t xs
  At' t (Reversed' _ xs) i -> Just $ At' t xs (Minus' (Minus' (Len' t xs) i) Lit1)
  Sum' (Reversed' _ xs) -> Just $ Sum' xs
  Product' (Reversed' _ xs) -> Just $ Product' xs
  Max1' t (Reversed' _ xs) -> Just $ Max1' t xs
  Min1' t (Reversed' _ xs) -> Just $ Min1' t xs
  ArgMin' t (Reversed' _ xs) -> Just $ ArgMin' t xs
  ArgMax' t (Reversed' _ xs) -> Just $ ArgMax' t xs
  All' (Reversed' _ xs) -> Just $ All' xs
  Any' (Reversed' _ xs) -> Just $ Any' xs
  -- reduce `Sorted`
  Len' t (Sorted' _ xs) -> Just $ Len' t xs
  Sum' (Sorted' _ xs) -> Just $ Sum' xs
  Product' (Sorted' _ xs) -> Just $ Product' xs
  Max1' t (Sorted' _ xs) -> Just $ Max1' t xs
  Min1' t (Sorted' _ xs) -> Just $ Min1' t xs
  All' (Sorted' _ xs) -> Just $ All' xs
  Any' (Sorted' _ xs) -> Just $ Any' xs
  -- reduce `Map`
  Len' _ (Map' t1 _ _ xs) -> Just $ Len' t1 xs
  At' _ (Map' t1 _ f xs) i -> Just $ App f (At' t1 xs i)
  Sum' (Map' t1 _ (Lam x _ e) xs) | x `isUnusedVar` e -> Just $ Mult' (Len' t1 xs) e
  Sum' (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Negate' (Sum' (Map' t1 t2 (Lam x t e) xs))
  Sum' (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) -> Just $ Plus' (Sum' (Map' t1 t2 (Lam x t e1) xs)) (Sum' (Map' t1 t2 (Lam x t e2) xs))
  Sum' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ Mult' e1 (Sum' (Map' t1 t2 (Lam x t e2) xs))
  Sum' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ Mult' e2 (Sum' (Map' t1 t2 (Lam x t e1) xs))
  Product' (Map' t1 _ (Lam x _ e) xs) | x `isUnusedVar` e -> Just $ Pow' e (Len' t1 xs)
  Product' (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Mult' (Pow' (Negate' Lit0) (Len' t1 xs)) (Product' (Map' t1 t2 (Lam x t e) xs))
  Product' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) -> Just $ Mult' (Product' (Map' t1 t2 (Lam x t e1) xs)) (Product' (Map' t1 t2 (Lam x t e2) xs))
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

reduceFoldBuild :: Monad m => RewriteRule m
reduceFoldBuild = simpleRewriteRule $ \case
  Foldl' _ t (Lam2 x1 t1 x2 _ body) x (Range1' n) | x2 `isUnusedVar` body -> Just $ Iterate' t n (Lam x1 t1 body) x
  Len' _ (Range1' n) -> Just n
  At' _ (Range1' _) i -> Just i
  Sum' (Range1' n) -> Just $ FloorDiv' (Mult' n (Minus' n Lit1)) Lit2
  Sum' (Map' _ _ (Lam x _ (Mult' x' x'')) (Range1' n)) | x' == Var x && x'' == Var x -> Just $ FloorDiv' (Mult' n (Mult' (Minus' n Lit1) (Minus' (Mult' Lit2 n) Lit1))) (Lit (LitInt 6))
  Sum' (Map' _ _ (Lam x _ (Mult' x' (Mult' x'' x'''))) (Range1' n)) | x' == Var x && x'' == Var x && x''' == Var x -> Just $ FloorDiv' (Mult' n (Mult' n (Mult' (Minus' n Lit1) (Minus' n Lit1)))) (Lit (LitInt 4))
  Product' (Range1' n) -> Just $ If' IntTy (Equal' IntTy n Lit0) Lit1 Lit0
  Max1' _ (Range1' n) -> Just $ Minus' n Lit1
  Min1' _ (Range1' _) -> Just Lit0
  ArgMax' _ (Range1' n) -> Just $ Minus' n Lit1
  ArgMin' _ (Range1' _) -> Just Lit0
  _ -> Nothing

reduceFold :: Monad m => RewriteRule m
reduceFold = simpleRewriteRule $ \case
  Iterate' _ k (Lam x _ (MatAp' n _ f (Var x'))) v | x `isUnusedVar` f && x == x' -> Just $ MatAp' n n (MatPow' n f k) v
  _ -> Nothing

rule :: MonadAlpha m => RewriteRule m
rule =
  mconcat
    [ reduceBuild,
      reduceMapMap,
      reduceFoldMap,
      reduceFoldBuild,
      reduceFold
    ]

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` does short cut fusion.
--
-- * This function is mainly for polymorphic reductions. This dosn't do much about concrete things, e.g., arithmetical operations.
--
-- == References
--
-- * <https://wiki.haskell.org/Short_cut_fusion Short cut fusion - HaskellWiki>
-- * <https://wiki.haskell.org/Correctness_of_short_cut_fusion Correctness of short cut fusion - HaskellWiki>
-- * <https://qiita.com/autotaker1984/items/5ec0bbd5a44e146dbada GHCの融合変換を理解する(前編) - Qiita>
--
-- == List of related builtin functions
--
-- \[
--     \newcommand\int{\mathbf{int}}
--     \newcommand\bool{\mathbf{bool}}
--     \newcommand\list{\mathbf{list}}
-- \]
--
-- === Build functions
--
-- * `Nil` \(: \forall \alpha. \list(\alpha)\)
-- * `Cons` \(: \forall \alpha. \alpha \to \list(\alpha) \to \list(\alpha)\)
-- * `Range1` \(: \int \to \list(\int)\)
-- * `Range2` \(: \int \to \int \to \list(\int)\)
-- * `Range3` \(: \int \to \int \to \int \to \list(\int)\)
-- * `Tabulate` \(: \forall \alpha. \int \to (\int \to \alpha) \to \list(\alpha)\)
--
-- === Map functions
--
-- * `SetAt` \(: \forall \alpha. \list(\alpha) \to \int \to \alpha \to \list(\alpha)\)
-- * `Scanl` \(: \forall \alpha \beta. (\beta \to \alpha \to \beta) \to \beta \to \list(\alpha) \to \list(\beta)\)
-- * `Map` \(: \forall \alpha \beta. (\alpha \to \beta) \to \list(\alpha) \to \list(\beta)\)
-- * `Filter` \(: \forall \alpha \beta. (\alpha \to \bool) \to \list(\alpha) \to \list(\beta)\)
-- * `Reversed` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
-- * `Sorted` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
-- * `List` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
--
-- === Fold functions
--
-- * `Len` \(: \forall \alpha. \list(\alpha) \to \int\)
-- * `At` \(: \forall \alpha. \list(\alpha) \to \int \to \alpha\)
-- * `Elem` \(: \forall \alpha. \alpha \to \list(\alpha) \to \bool\)
-- * `Max1` \(: \forall \alpha. \list(\alpha) \to \alpha\)
-- * `Min1` \(: \forall \alpha. \list(\alpha) \to \alpha\)
-- * `ArgMax` \(: \forall \alpha. \list(\alpha) \to \int\)
-- * `ArgMin` \(: \forall \alpha. \list(\alpha) \to \int\)
-- * `Sum` \(: \list(\int) \to \int\)
-- * `Product` \(: \list(\int) \to \int\)
-- * `ModSum` \(: \list(\int) \to \int \to \int\)
-- * `ModProduct` \(: \list(\int) \to \int \to \int\)
-- * `All` \(: \list(\bool) \to \bool\)
-- * `Any` \(: \list(\bool) \to \bool\)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.ShortCutFusion" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
