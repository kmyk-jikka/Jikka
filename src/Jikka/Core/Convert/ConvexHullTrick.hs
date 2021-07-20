{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.ConvexHullTrick
-- Description : uses convex hull trick. / convex hull trick を使います。
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
module Jikka.Core.Convert.ConvexHullTrick
  ( run,

    -- * internal rules
    rule,
    parseLinearFunctionBody,
    parseLinearFunctionBody',
  )
where

import Control.Monad.Trans.Maybe
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

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

-- | This is something commutative because only one kind of @c@ is allowed.
plusPair :: (ArithmeticalExpr, ArithmeticalExpr) -> (ArithmeticalExpr, ArithmeticalExpr) -> Maybe (ArithmeticalExpr, ArithmeticalExpr)
plusPair (a1, c1) (a2, _) | isZeroArithmeticalExpr a2 = Just (a1, c1)
plusPair (a1, c1) (_, c2) | isZeroArithmeticalExpr c2 = Just (a1, c1)
plusPair (a1, _) (a2, c2) | isZeroArithmeticalExpr a1 = Just (a2, c2)
plusPair (_, c1) (a2, c2) | isZeroArithmeticalExpr c1 = Just (a2, c2)
plusPair (a1, c1) (a2, c2) =
  let (k1, c1') = splitConstantFactorArithmeticalExpr c1
      (k2, c2') = splitConstantFactorArithmeticalExpr c2
      a1' = multArithmeticalExpr (integerArithmeticalExpr k1) a1
      a2' = multArithmeticalExpr (integerArithmeticalExpr k2) a2
   in if c1' == c2'
        then Just (plusArithmeticalExpr a1' a2', c1')
        else Nothing

sumPairs :: [(ArithmeticalExpr, ArithmeticalExpr)] -> Maybe (ArithmeticalExpr, ArithmeticalExpr)
sumPairs = foldr (\e1 e2 -> plusPair e1 =<< e2) (Just (integerArithmeticalExpr 1, integerArithmeticalExpr 0))

-- | `parseLinearFunctionBody'` parses the body of a linear function which can be decomposed to convex hull trick.
-- @parseLinearFunctionBody' f i j k e@ finds a 4-tuple @a, b, c, d@ where @e = a(f[j], j) c(f[< i + k], i) + b(f[j], j) + d(f[< i + k], i)@.
--
-- TODO: What is the relation between @j@ and @k@?
parseLinearFunctionBody' :: VarName -> VarName -> Integer -> VarName -> Expr -> Maybe (Expr, Expr, Expr, Expr)
parseLinearFunctionBody' f i k j e = result <$> go e
  where
    result (a, c, b, d) =
      let (k, a') = splitConstantFactorArithmeticalExpr a
          c' = multArithmeticalExpr (integerArithmeticalExpr k) c
       in (formatArithmeticalExpr a', formatArithmeticalExpr c', formatArithmeticalExpr b, formatArithmeticalExpr d)
    go = \case
      Negate' e -> do
        (a, c, b, d) <- go e
        return (a, negateArithmeticalExpr c, negateArithmeticalExpr b, negateArithmeticalExpr d)
      Plus' e1 e2 -> do
        (a1, c1, b1, d1) <- go e1
        (a2, c2, b2, d2) <- go e2
        (a, c) <- plusPair (a1, c1) (a2, c2)
        return (a, c, plusArithmeticalExpr b1 b2, plusArithmeticalExpr d1 d2)
      Minus' e1 e2 -> do
        (a1, c1, b1, d1) <- go e1
        (a2, c2, b2, d2) <- go e2
        (a, c) <- plusPair (a1, c1) (negateArithmeticalExpr a2, c2)
        return (a, c, minusArithmeticalExpr b1 b2, minusArithmeticalExpr d1 d2)
      Mult' e1 e2 -> do
        (a1, c1, b1, d1) <- go e1
        (a2, c2, b2, d2) <- go e2
        (a, c) <-
          sumPairs
            [ (multArithmeticalExpr a1 a2, multArithmeticalExpr c1 c2),
              (multArithmeticalExpr b2 a1, c1),
              (multArithmeticalExpr b1 a2, c2),
              (a1, multArithmeticalExpr c1 d2),
              (a2, multArithmeticalExpr c2 d1),
              (b2, d1),
              (b1, d2)
            ]
        return (a, c, multArithmeticalExpr b1 b2, multArithmeticalExpr d1 d2)
      e
        | f `isUnusedVar` e && j `isUnusedVar` e ->
          -- NOTE: Put constants to @d@ and simplify @a, b@
          return (integerArithmeticalExpr 1, integerArithmeticalExpr 0, integerArithmeticalExpr 0, parseArithmeticalExpr e)
      e
        | f `isUnusedVar` e && i `isUnusedVar` e ->
          return (integerArithmeticalExpr 1, integerArithmeticalExpr 0, parseArithmeticalExpr e, integerArithmeticalExpr 0)
      e@(At' _ (Var x) index) | x == f -> case unNPlusKPattern (parseArithmeticalExpr index) of
        Just (i', k') | i' == i && k' < k -> do
          return (integerArithmeticalExpr 1, integerArithmeticalExpr 0, integerArithmeticalExpr 0, parseArithmeticalExpr e)
        Just (j', 0) | j' == j -> do
          return (integerArithmeticalExpr 1, integerArithmeticalExpr 0, parseArithmeticalExpr e, integerArithmeticalExpr 0)
        _ -> Nothing
      _ -> Nothing

parseLinearFunctionBody :: MonadAlpha m => VarName -> VarName -> Integer -> Expr -> m (Maybe (Expr, Expr, Expr, Expr, Expr))
parseLinearFunctionBody f i k = runMaybeT . go
  where
    go = \case
      Min1' _ (Map' _ _ (Lam j _ step) (Range1' size)) -> case unNPlusKPattern (parseArithmeticalExpr size) of
        Just (i', k') | i' == i && k' == k -> do
          (a, b, c, d) <- hoistMaybe $ parseLinearFunctionBody' f i k j step
          -- raname @j@ to @i@
          a <- lift $ substitute j (Var i) a
          c <- lift $ substitute j (Var i) c
          return (LitInt' 1, a, b, c, d)
        _ -> hoistMaybe Nothing
      Max1' _ (Map' _ _ (Lam j _ step) (Range1' size)) -> case unNPlusKPattern (parseArithmeticalExpr size) of
        Just (i', k') | i' == i && k' == k -> do
          (a, b, c, d) <- hoistMaybe $ parseLinearFunctionBody' f i k j step
          -- raname @j@ to @i@
          a <- lift $ substitute j (Var i) a
          c <- lift $ substitute j (Var i) c
          return (LitInt' (-1), a, Negate' b, Negate' c, d)
        _ -> hoistMaybe Nothing
      Negate' e -> do
        (sign, a, b, c, d) <- go e
        return (Negate' sign, a, b, c, Negate' d)
      Plus' e1 e2 | isConstantTimeExpr e2 -> do
        (sign, a, b, c, d) <- go e1
        return (sign, a, b, c, Plus' d e2)
      Plus' e1 e2 | isConstantTimeExpr e1 -> do
        (sign, a, b, c, d) <- go e2
        return (sign, a, b, c, Plus' e1 d)
      Minus' e1 e2 | isConstantTimeExpr e2 -> do
        (sign, a, b, c, d) <- go e1
        return (sign, a, b, c, Minus' d e2)
      Minus' e1 e2 | isConstantTimeExpr e1 -> do
        (sign, a, b, c, d) <- go e2
        return (Negate' sign, a, b, c, Minus' e1 d)
      Mult' e1 e2 | isConstantTimeExpr e2 -> do
        (sign, a, b, c, d) <- go e1
        return (Mult' sign e2, a, b, c, Mult' d e2)
      Mult' e1 e2 | isConstantTimeExpr e1 -> do
        (sign, a, b, c, d) <- go e2
        return (Mult' e1 sign, a, b, c, Mult' e1 d)
      _ -> hoistMaybe Nothing

rule :: MonadAlpha m => RewriteRule m
rule = RewriteRule $ \_ -> \case
  -- foldl (fun f i -> setat f index(i) step(f, i)) base (range n)
  Foldl' IntTy (ListTy IntTy) (Lam2 f _ i _ (SetAt' _ (Var f') index step)) base (Range1' n) | f' == f && f `isUnusedVar` index -> runMaybeT $ do
    -- index(i) = i + k
    k <- hoistMaybe $ case unNPlusKPattern (parseArithmeticalExpr index) of
      Just (i', k) | i' == i -> Just k
      _ -> Nothing
    -- step(f, i) = sign(f, i) * min (map (fun j -> a(f, j) c(f, i) + b(f, j)) (range (i + k))) + d(f, i)
    (sign, a, c, b, d) <- MaybeT $ parseLinearFunctionBody f i k step
    x <- lift genVarName'
    y <- lift genVarName'
    f' <- lift $ genVarName f
    let ts = [ConvexHullTrickTy, ListTy IntTy]
    -- base' = (empty, base)
    let base' = uncurryApp (Tuple' ts) [ConvexHullTrickInit', base]
    -- step' = fun (cht, f) i ->
    --     let f' = setat f index(i) (min cht f[i + k] + c(i))
    --     in let cht' = update cht a(i) b(i)
    --     in (cht', f')
    let step' =
          Lam2 x (TupleTy ts) i IntTy $
            Let f (ListTy IntTy) (Proj' ts 1 (Var x)) $
              Let y ConvexHullTrickTy (ConvexHullTrickInsert' (Proj' ts 0 (Var x)) a b) $
                Let f' (ListTy IntTy) (SetAt' IntTy (Var f) index (Plus' (Mult' sign (ConvexHullTrickGetMin' (Var y) c)) d)) $
                  uncurryApp (Tuple' ts) [Var y, Var f']
    -- proj 1 (foldl step' base' (range (n - 1)))
    return $ Proj' ts 1 (Foldl' IntTy (TupleTy ts) step' base' (Range1' n))
  _ -> return Nothing

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` optimizes a DP which has the recurrence relation
-- \[
--     \mathrm{dp}(i) = \min a(j) x(i) + b(j) \lbrace \mid j \lt i \rbrace + c(i)
-- \] where only appropriate elements of \(\mathrm{dp}\) are used in \(a, x, b, c\).
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.ConvexHullTrick" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
