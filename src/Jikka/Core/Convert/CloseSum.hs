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
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

reduceSum :: MonadAlpha m => RewriteRule m
reduceSum =
  let return' = return . Just
   in makeRewriteRule "reduceSum" $ \_ -> \case
        Sum' xs -> case xs of
          -- reduce list build functions
          Nil' _ -> return' Lit0
          Cons' _ x xs -> return' $ Plus' x (Sum' xs)
          Range1' n -> return' $ FloorDiv' (Mult' n (Minus' n Lit1)) Lit2
          -- reduce list map functions
          Reversed' _ xs -> return' $ Sum' xs
          Sorted' _ xs -> return' $ Sum' xs
          Filter' _ g (Map' t1 _ f xs) -> do
            x <- genVarName'
            let h = Lam x t1 (If' IntTy (App g (App f (Var x))) (App f (Var x)) Lit0)
            return' $ Sum' (Map' t1 IntTy h xs)
          Map' t1 IntTy (Lam x _ body) xs -> case (body, xs) of
            (e, xs) | x `isUnusedVar` e -> return' $ Mult' (Len' t1 xs) e
            (body, Range1' n) | body == Var x -> return' $ FloorDiv' (Mult' n (Minus' n Lit1)) Lit2
            (body, Range1' n) | body == Mult' (Var x) (Var x) || body == Pow' (Var x) (LitInt' 2) -> return' $ FloorDiv' (Mult' n (Mult' (Minus' n Lit1) (Minus' (Mult' Lit2 (Var x)) Lit1))) (LitInt' 6)
            (Negate' e, xs) -> return' $ Negate' (Sum' (Map' t1 IntTy (Lam x t1 e) xs))
            (Plus' e1 e2, xs) -> return' $ Plus' (Sum' (Map' t1 IntTy (Lam x t1 e1) xs)) (Sum' (Map' t1 IntTy (Lam x t1 e2) xs))
            (Minus' e1 e2, xs) -> return' $ Minus' (Sum' (Map' t1 IntTy (Lam x t1 e1) xs)) (Sum' (Map' t1 IntTy (Lam x t1 e2) xs))
            (Mult' e1 e2, xs) | x `isUnusedVar` e1 -> return' $ Mult' e1 (Sum' (Map' t1 IntTy (Lam x t1 e2) xs))
            (Mult' e1 e2, xs) | x `isUnusedVar` e2 -> return' $ Mult' e2 (Sum' (Map' t1 IntTy (Lam x t1 e1) xs))
            _ -> return Nothing
          -- others
          _ -> return Nothing
        _ -> return Nothing

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
  let return' = return . Just
   in makeRewriteRule "reduceModSum" $ \_ -> \case
        ModSum' xs m -> case xs of
          -- the corner case
          _ | m == Lit1 -> return' Lit0
          -- reduce list build functions
          Nil' _ -> return' Lit0
          Cons' _ x xs -> return' $ ModPlus' x (ModSum' xs m) m
          Range1' n -> return' $ ModMult' (ModMult' n (ModPlus' n Lit1 m) m) (ModInv' Lit2 m) m
          -- reduce list map functions
          Reversed' _ xs -> return' $ ModSum' xs m
          Sorted' _ xs -> return' $ ModSum' xs m
          Filter' _ g (Map' t1 _ f xs) -> do
            x <- genVarName'
            let h = Lam x t1 (If' IntTy (App g (App f (Var x))) (App f (Var x)) Lit0)
            return' $ ModSum' (Map' t1 IntTy h xs) m
          Map' t1 IntTy (Lam x _ body) xs -> do
            let go body = case (body, xs) of
                  (e, xs) | x `isUnusedVar` e -> return' $ ModMult' (Len' t1 xs) e m
                  (body, Range1' n) | body == Var x -> return' $ ModMult' (ModMult' n (ModMinus' n Lit1 m) m) (ModInv' Lit2 m) m
                  (body, Range1' n) | body == ModMult' (Var x) (Var x) m || body == ModPow' (Var x) (LitInt' 2) m -> return' $ ModMult' (ModMult' n (ModMult' (ModMinus' n Lit1 m) (ModMinus' (ModMult' Lit2 n m) Lit1 m) m) m) (ModInv' (LitInt' 6) m) m
                  (ModNegate' e m', xs) | m' == m -> return' $ ModNegate' (ModSum' (Map' t1 IntTy (Lam x t1 e) xs) m) m
                  (ModPlus' e1 e2 m', xs) | m' == m -> return' $ ModPlus' (ModSum' (Map' t1 IntTy (Lam x t1 e1) xs) m) (ModSum' (Map' t1 IntTy (Lam x t1 e2) xs) m) m
                  (ModMinus' e1 e2 m', xs) | m' == m -> return' $ ModMinus' (ModSum' (Map' t1 IntTy (Lam x t1 e1) xs) m) (ModSum' (Map' t1 IntTy (Lam x t1 e2) xs) m) m
                  (ModMult' e1 e2 m', xs) | x `isUnusedVar` e1 && m' == m -> return' $ ModMult' e1 (ModSum' (Map' t1 IntTy (Lam x t1 e2) xs) m) m
                  (ModMult' e1 e2 m', xs) | x `isUnusedVar` e2 && m' == m -> return' $ ModMult' e2 (ModSum' (Map' t1 IntTy (Lam x t1 e1) xs) m) m
                  _ -> return Nothing
            case body of
              FloorMod' body m' | m' == m -> go body -- We shouldn't remove FloorMod not to introduce loops of rewrite rules between Jikka.Core.Convert.PropagateMod.
              _ -> go body
          -- others
          _ -> return Nothing
        _ -> return Nothing

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
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
