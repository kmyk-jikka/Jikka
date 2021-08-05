{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.MakeScanl
-- Description : converts @foldl@ on lists to @scanl@. / リスト上の @foldl@ を @scanl@ に変換します。
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
module Jikka.Core.Convert.MakeScanl
  ( run,

    -- * internal rules
    rule,
    reduceScanlBuild,
    reduceFoldlSetAtRecurrence,
    reduceFoldlSetAtAccumulation,
    reduceFoldlSetAtGeneric,
    getRecurrenceFormulaBase,
    getRecurrenceFormulaStep1,
    getRecurrenceFormulaStep,
  )
where

import Control.Monad.Trans.Maybe
import Data.List
import qualified Data.Map as M
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.ArithmeticalExpr
import Jikka.Core.Language.Beta
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

-- |
-- == List of builtin functions which are reduced
--
-- * `Nil` \(: \forall \alpha. \list(\alpha)\)
-- * `Cons` \(: \forall \alpha. \alpha \to \list(\alpha) \to \list(\alpha)\)
-- * `Scanl` \(: \forall \alpha \beta. (\beta \to \alpha \to \beta) \to \beta \to \list(\alpha) \to \list(\beta)\)
reduceScanlBuild :: Monad m => RewriteRule m
reduceScanlBuild = simpleRewriteRule "reduceScanlBuild" $ \case
  Scanl' _ t2 _ init (Nil' _) -> Just $ Cons' t2 init (Nil' t2)
  Scanl' t1 t2 f init (Cons' _ x xs) -> Just $ Cons' t2 init (Scanl' t1 t2 f (App2 f init x) xs)
  _ -> Nothing

-- | `getRecurrenceFormulaStep1` removes `At` in @body@.
getRecurrenceFormulaStep1 :: MonadAlpha m => Integer -> Type -> VarName -> VarName -> Expr -> m (Maybe Expr)
getRecurrenceFormulaStep1 shift t a i body = do
  x <- genVarName a
  let proj k =
        if toInteger shift + k == 0
          then Just $ Var x
          else Nothing
  let go :: Expr -> Maybe Expr
      go = \case
        At' _ (Var a') i' | a' == a -> case unNPlusKPattern (parseArithmeticalExpr i') of
          Just (i', k) | i' == i -> proj k
          _ -> Nothing
        Var x -> if x == a then Nothing else Just (Var x)
        Lit lit -> Just $ Lit lit
        App f e -> App <$> go f <*> go e
        Lam x t e -> Lam x t <$> if x == a then Just e else go e
        Let x t e1 e2 -> Let x t <$> go e1 <*> if x == a then Just e2 else go e2
        Assert f e -> Assert <$> go f <*> go e
  return $ case go body of
    Just body -> Just $ Lam2 x t i IntTy body
    Nothing -> Nothing

-- | `getRecurrenceFormulaStep` replaces `At` in @body@ with `Proj`.
getRecurrenceFormulaStep :: MonadAlpha m => Integer -> Integer -> Type -> VarName -> VarName -> Expr -> m (Maybe Expr)
getRecurrenceFormulaStep shift size t a i body = do
  x <- genVarName a
  let ts = replicate (fromInteger size) t
  let proj k =
        if 0 <= toInteger shift + k && toInteger shift + k < toInteger size
          then Just $ Proj' ts (shift + k) (Var x)
          else Nothing
  let go :: Expr -> Maybe Expr
      go = \case
        At' _ (Var a') i' | a' == a -> case unNPlusKPattern (parseArithmeticalExpr i') of
          Just (i', k) | i' == i -> proj k
          _ -> Nothing
        Var x -> if x == a then Nothing else Just (Var x)
        Lit lit -> Just $ Lit lit
        App f e -> App <$> go f <*> go e
        Lam x t e -> Lam x t <$> if x == a then Just e else go e
        Let x t e1 e2 -> Let x t <$> go e1 <*> if x == a then Just e2 else go e2
        Assert f e -> Assert <$> go f <*> go e
  return $ case go body of
    Just body -> Just $ Lam2 x (TupleTy ts) i IntTy (uncurryApp (Tuple' ts) (map (\i -> Proj' ts i (Var x)) [1 .. size - 1] ++ [body]))
    Nothing -> Nothing

-- |
-- * This assumes that `Range2` and `Range3` are already converted to `Range1` (`Jikka.Core.Convert.ShortCutFusion`).
-- * This assumes that combinations `Foldl` and `Map` squashed (`Jikka.Core.Convert.ShortCutFusion`).
-- * This assumes that constants are already folded (`Jikka.Core.Convert.ConstantFolding`).
reduceFoldlSetAtRecurrence :: MonadAlpha m => RewriteRule m
reduceFoldlSetAtRecurrence = makeRewriteRule "reduceFoldlSetAtRecurrence" $ \_ -> \case
  -- foldl (fun a i -> setat a index(i) step(a, i)) base indices
  Foldl' _ (ListTy t2) (Lam2 a _ i _ (SetAt' _ (Var a') index step)) base indices | a' == a && a `isUnusedVar` index -> runMaybeT $ do
    -- index(i) = i + k
    k <- hoistMaybe $ case unNPlusKPattern (parseArithmeticalExpr index) of
      Just (i', k) | i' == i -> Just k
      _ -> Nothing
    -- indices = range n
    n <- hoistMaybe $ case indices of
      Range1' n -> Just n -- We can do this because foldl-map combinations are already reduced.
      _ -> Nothing
    -- init = setat (k-1) a_{k-1} (... (setat 0 a_0 (range n)) ...)
    (base, _) <- return $ getRecurrenceFormulaBase base -- TODO: care about cases when base is longer than indices
    case base of
      [] ->
        if k == 0 && a `isUnusedVar` step
          then return $ Map' IntTy t2 (Lam i IntTy step) (Range1' n)
          else hoistMaybe Nothing
      [base] -> do
        step <- MaybeT $ getRecurrenceFormulaStep1 (- 1 + fromInteger k) t2 a i step
        return $ Scanl' IntTy t2 step base (Range1' n)
      _ -> do
        let ts = replicate (length base) t2
        let base' = uncurryApp (Tuple' ts) base
        step <- MaybeT $ getRecurrenceFormulaStep (- genericLength base + k) (genericLength base) t2 a i step
        x <- lift (genVarName a)
        return $ foldr (Cons' t2) (Map' (TupleTy ts) t2 (Lam x (TupleTy ts) (Proj' ts (genericLength base - 1) (Var x))) (Scanl' IntTy (TupleTy ts) step base' (Range1' n))) (init base)
  _ -> return Nothing

-- | `checkAccumulationFormulaStep` checks that all `At` in @body@ about @a@ are @At a i@.
checkAccumulationFormulaStep :: VarName -> VarName -> Expr -> Bool
checkAccumulationFormulaStep a i = go
  where
    go = \case
      At' _ (Var a') i' | a' == a -> case i' of
        Var i' | i' == i -> True
        _ -> False
      Var x -> x /= a
      Lit _ -> True
      App f e -> go f && go e
      Lam x _ e -> x == a || go e
      Let x _ e1 e2 -> go e1 && (x == a || go e2)
      Assert e1 e2 -> go e1 && go e2

-- |
-- * This assumes that `Range2` and `Range3` are already converted to `Range1` (`Jikka.Core.Convert.ShortCutFusion`).
-- * This assumes that combinations `Foldl` and `Map` squashed (`Jikka.Core.Convert.ShortCutFusion`).
-- * This assumes that constants are already folded (`Jikka.Core.Convert.ConstantFolding`).
reduceFoldlSetAtAccumulation :: MonadAlpha m => RewriteRule m
reduceFoldlSetAtAccumulation = makeRewriteRule "reduceFoldlSetAtAccumulation" $ \_ -> \case
  -- foldl (fun a i -> setat a index() step(a, i)) base indices
  Foldl' _ (ListTy t2) (Lam2 a _ i _ (SetAt' _ (Var a') index step)) base indices | a' == a && a `isUnusedVar` index && i `isUnusedVar` index -> runMaybeT $ do
    -- step(a, i) = op (at a index()) step'(a, i)
    (accumulate, step) <- hoistMaybe $ case step of
      Max2' t (At' _ (Var a') index') step | a' == a && index' == index -> Just (Max1' t, step)
      Min2' t (At' _ (Var a') index') step | a' == a && index' == index -> Just (Min1' t, step)
      Plus' (At' _ (Var a') index') step | a' == a && index' == index -> Just (Sum', step)
      Mult' (At' _ (Var a') index') step | a' == a && index' == index -> Just (Product', step)
      ModPlus' (At' _ (Var a') index') step m | a' == a && index' == index && a `isUnusedVar` m && i `isUnusedVar` m -> Just ((`ModSum'` m), step)
      ModMult' (At' _ (Var a') index') step m | a' == a && index' == index && a `isUnusedVar` m && i `isUnusedVar` m -> Just ((`ModProduct'` m), step)
      _ -> Nothing
    -- indices = range (index())
    guard $ indices == Range1' index
    -- step'(a, i) = step''(at a i)
    guard $ checkAccumulationFormulaStep a i step
    step <- lift $ substitute a base step
    return $ SetAt' t2 base index (accumulate (Map' IntTy t2 (Lam i IntTy step) (Range1' index)))
  _ -> return Nothing

-- | `checkGenericRecurrenceFormulaStep` checks that all `At` in @body@ about @a@ have indices less than @i + k@.
checkGenericRecurrenceFormulaStep :: VarName -> VarName -> Integer -> Expr -> Bool
checkGenericRecurrenceFormulaStep a = \i k -> go (M.fromList [(i, k - 1)])
  where
    -- (i, k) in env menas a[i + k] is accessible but a[i + k + 1] is not.
    go :: M.Map VarName Integer -> Expr -> Bool
    go env = \case
      At' _ (Var a') i | a' == a -> case unNPlusKPattern (parseArithmeticalExpr i) of
        Just (i, k) -> case M.lookup i env of
          Just limit -> k <= limit
          Nothing -> False
        _ -> False
      Map' _ _ (Lam j _ body) (Range1' n) | j /= a -> case unNPlusKPattern (parseArithmeticalExpr n) of
        Just (i, k) -> case M.lookup i env of
          Just limit -> go (M.insert j (limit - k + 1) env) body
          Nothing -> go env body && go env n
        _ -> go env body && go env n
      Var x -> x /= a
      Lit _ -> True
      App f e -> go env f && go env e
      Lam x _ e -> x == a || go env e
      Let x _ e1 e2 -> go env e1 && (x == a || go env e2)
      Assert e1 e2 -> go env e1 && go env e2

reduceFoldlSetAtGeneric :: MonadAlpha m => RewriteRule m
reduceFoldlSetAtGeneric = makeRewriteRule "reduceFoldlSetAtGeneric" $ \_ -> \case
  -- foldl (fun a i -> setat a index(i) step(a, i)) base indices
  Foldl' _ (ListTy t2) (Lam2 a _ i _ (SetAt' _ (Var a') index step)) base indices | a' == a && a `isUnusedVar` index -> runMaybeT $ do
    -- index(i) = i + k
    k <- hoistMaybe $ case unNPlusKPattern (parseArithmeticalExpr index) of
      Just (i', k) | i' == i -> Just k
      _ -> Nothing
    -- indices = range n
    n <- hoistMaybe $ case indices of
      Range1' n -> Just n -- We can do this because foldl-map combinations are already reduced.
      _ -> Nothing
    -- base = setat (k - 1) a_{k - 1} (... (setat 0 a_0 (range n)) ...)
    (base, _) <- return $ getRecurrenceFormulaBase base -- TODO: care about cases when base is longer than indices
    -- step(a, i) = step(a[0], a[1], ..., a[i + k - 1], i)
    guard $ checkGenericRecurrenceFormulaStep a i k step
    step <- lift $ substitute i (Minus' (Len' t2 (Var a)) (LitInt' k)) step
    return $ Build' t2 (Lam a (ListTy t2) step) (foldl (Snoc' t2) (Nil' t2) base) n
  _ -> return Nothing

rule :: MonadAlpha m => RewriteRule m
rule =
  mconcat
    [ reduceScanlBuild,
      reduceFoldlSetAtRecurrence,
      reduceFoldlSetAtAccumulation,
      reduceFoldlSetAtGeneric
    ]

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` replaces `Foldl` with `Scanl`.
--
-- == Example
--
-- Before:
--
-- > let xs = range n
-- > xs[0] <- 0
-- > xs[1] <- 1
-- > foldl (fun a i -> do
-- >    xs[i + 2] <- xs[i] + xs[i + 1]
-- >    xs
-- > ) xs (range (n - 2))
--
-- After:
--
-- > 0 : map snd (
-- >    scanl (fun a i -> (snd a, fst a + snd a))
-- >          (0, 1)
-- >          (range (n - 2)))
--
-- == List of builtin functions which are reduced
--
-- === Build functions
--
-- * `Nil` \(: \forall \alpha. \list(\alpha)\)
-- * `Cons` \(: \forall \alpha. \alpha \to \list(\alpha) \to \list(\alpha)\)
-- * `Range1` \(: \int \to \list(\int)\)
-- * `Build` \(: \forall \alpha. (\list(\alpha) \to \alpha) \to \list(\alpha) \to \int \to \list(\alpha)\)
--
-- === Map functions
--
-- * `Scanl` \(: \forall \alpha \beta. (\beta \to \alpha \to \beta) \to \beta \to \list(\alpha) \to \list(\beta)\)
-- * `SetAt` \(: \forall \alpha. \list(\alpha) \to \int \to \alpha \to \list(\alpha)\)
--
-- === Fold functions
--
-- * `Foldl` \(: \forall \alpha \beta. (\beta \to \alpha \to \beta) \to \beta \to \list(\alpha) \to \beta\)
-- * `At` \(: \forall \alpha. \list(\alpha) \to \int \to \alpha\)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.MakeScanl" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
