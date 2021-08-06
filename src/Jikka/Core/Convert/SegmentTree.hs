{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Jikka.Core.Convert.SegmentTree
-- Description : uses segment trees. / セグメント木を利用します。
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
module Jikka.Core.Convert.SegmentTree
  ( run,

    -- * internal rules
    rule,
    reduceCumulativeSum,
    reduceMin,
  )
where

import Control.Arrow
import Control.Monad.Trans.Maybe
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.Alpha as Alpha
import Jikka.Core.Language.Beta
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

pattern CumulativeSum t f e es <-
  ( \case
      Scanl' t t' (Lam2 x1 t'' x2 t''' (App (App f (Var x1')) (Var x2'))) e es
        | t == t' && t' == t'' && t'' == t''' && x1 == x1' && x1 `isUnusedVar` f && x2 == x2' && x2 `isUnusedVar` f -> Just (t, f, e, es)
      _ -> Nothing ->
      Just (t, f, e, es)
    )
  where
    CumulativeSum t f e es =
      let x1 = findUnusedVarName (VarName "y") f
          x2 = findUnusedVarName (VarName "x") f
       in Scanl' t t (Lam2 x1 t x2 t (App (App f (Var x1)) (Var x2))) e es

pattern CumulativeSumFlip t f e es <-
  ( \case
      Scanl' t t' (Lam2 x1 t'' x2 t''' (App (App f (Var x2')) (Var x1'))) e es
        | t == t' && t' == t'' && t'' == t''' && x2 == x2' && x2 `isUnusedVar` f && x1 == x1' && x1 `isUnusedVar` f -> Just (t, f, e, es)
      _ -> Nothing ->
      Just (t, f, e, es)
    )
  where
    CumulativeSumFlip t f e es =
      let x1 = findUnusedVarName (VarName "y") f
          x2 = findUnusedVarName (VarName "x") f
       in Scanl' t t (Lam2 x1 t x2 t (App (App f (Var x2)) (Var x1))) e es

builtinToSemigroup :: Builtin -> [Type] -> Maybe Semigroup'
builtinToSemigroup builtin ts = case (builtin, ts) of
  (Plus, []) -> Just SemigroupIntPlus
  (Min2, [IntTy]) -> Just SemigroupIntMin
  (Max2, [IntTy]) -> Just SemigroupIntMax
  (Gcd, []) -> Just SemigroupIntGcd
  (Lcm, []) -> Just SemigroupIntLcm
  _ -> Nothing

semigroupToBuiltin :: Semigroup' -> (Builtin, [Type])
semigroupToBuiltin = \case
  SemigroupIntPlus -> (Plus, [])
  SemigroupIntMin -> (Min2, [IntTy])
  SemigroupIntMax -> (Max2, [IntTy])
  SemigroupIntGcd -> (Gcd, [])
  SemigroupIntLcm -> (Lcm, [])

unCumulativeSum :: Expr -> Expr -> Maybe (Semigroup', Expr)
unCumulativeSum a = \case
  CumulativeSum _ (Lit (LitBuiltin op ts)) b a' | a' == a -> case builtinToSemigroup op ts of
    Just semigrp -> Just (semigrp, b)
    Nothing -> Nothing
  -- Semigroups must be commutative to use CumulativeSumFlip.
  CumulativeSumFlip _ (Lit (LitBuiltin op ts)) b a' | a' == a -> case builtinToSemigroup op ts of
    Just semigrp -> Just (semigrp, b)
    Nothing -> Nothing
  _ -> Nothing

listCumulativeSum :: Expr -> Expr -> [(Semigroup', Expr)]
listCumulativeSum a = mapMaybe (unCumulativeSum a) . listSubExprs

replaceWithSegtrees :: VarName -> [(Semigroup', Expr)] -> Expr -> Expr
replaceWithSegtrees a segtrees = go M.empty
  where
    go :: M.Map VarName (Expr, Expr, Semigroup') -> Expr -> Expr
    go env = \case
      At' _ (check env -> Just (e, b, semigrp)) i ->
        let e' = SegmentTreeGetRange' semigrp e (LitInt' 0) i
         in App2 (Lit (uncurry LitBuiltin (semigroupToBuiltin semigrp))) b e'
      Var x -> Var x
      Lit lit -> Lit lit
      App e1 e2 -> App (go env e1) (go env e2)
      Lam x t e -> Lam x t $ go (M.delete x env) e
      Let x t e1 e2 ->
        let e1' = go env e1
         in case check env e1' of
              Just (e1', b, semigrp) -> go (M.insert x (e1', b, semigrp) env) e2
              Nothing -> Let x t (go env e1) (go env e2)
    check :: M.Map VarName (Expr, Expr, Semigroup') -> Expr -> Maybe (Expr, Expr, Semigroup')
    check env = \case
      Var x -> M.lookup x env
      CumulativeSum _ (Lit (LitBuiltin op ts)) b (Var a') | a' == a -> case lookup (op, ts) (map (first semigroupToBuiltin) segtrees) of
        Just e -> Just (e, b, fromJust (builtinToSemigroup op ts))
        Nothing -> Nothing
      _ -> Nothing

-- | `reduceCumulativeSum` converts combinations of cumulative sums and array assignments to segment trees.
reduceCumulativeSum :: (MonadAlpha m, MonadError Error m) => RewriteRule m
reduceCumulativeSum = RewriteRule $ \_ -> \case
  -- foldl (fun a i -> setat a index(i) e(a, i)) base incides
  Foldl' t1 t2 (Lam2 a _ i _ (SetAt' t (Var a') index e)) base indices | a' == a && a `isUnusedVar` index -> runMaybeT $ do
    let sums = listCumulativeSum (Var a) e
    guard $ not (null sums)
    let semigrps = nub (sort (map fst sums))
    let ts = t2 : map SegmentTreeTy semigrps
    c <- lift $ genVarName a
    let proj i = Proj' ts i (Var c)
    let e' = replaceWithSegtrees a (zip semigrps (map proj [1 ..])) e
    guard $ e' /= e
    e' <- lift $ substitute a (proj 0) e'
    b' <- lift $ genVarName a
    let updateSegtrees i semigrp = SegmentTreeSetPoint' semigrp (proj i) index (At' t (Var b') index)
    let step = Lam2 c (TupleTy ts) i t1 (Let b' t2 (SetAt' t (proj 0) index e') (uncurryApp (Tuple' ts) (Var b' : zipWith updateSegtrees [1 ..] semigrps)))
    b <- lift $ genVarName a
    let base' = Var b : map (\semigrp -> SegmentTreeInitList' semigrp (Var b)) semigrps
    return $ Let b t2 base (Proj' ts 0 (Foldl' t1 (TupleTy ts) step (uncurryApp (Tuple' ts) base') indices))
  _ -> return Nothing

-- | `reduceFromMin` uses segment trees from accumulation of min/max which are not reducible to cumulative sums.
--
-- TODO: implement this
reduceMin :: MonadAlpha m => RewriteRule m
reduceMin = RewriteRule $ \_ -> \case
  _ -> return Nothing

rule :: (MonadAlpha m, MonadError Error m) => RewriteRule m
rule =
  mconcat
    [ reduceCumulativeSum,
      reduceMin
    ]

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.SegmentTree" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  prog <- Alpha.run prog
  postcondition $ do
    ensureWellTyped prog
  return prog
