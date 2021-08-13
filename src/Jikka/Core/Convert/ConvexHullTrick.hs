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
import Jikka.Core.Language.ArithmeticExpr
import Jikka.Core.Language.Beta
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

-- | This is something commutative because only one kind of @c@ is allowed.
plusPair :: (ArithmeticExpr, ArithmeticExpr) -> (ArithmeticExpr, ArithmeticExpr) -> Maybe (ArithmeticExpr, ArithmeticExpr)
plusPair (a1, c1) (a2, _) | isZeroArithmeticExpr a2 = Just (a1, c1)
plusPair (a1, c1) (_, c2) | isZeroArithmeticExpr c2 = Just (a1, c1)
plusPair (a1, _) (a2, c2) | isZeroArithmeticExpr a1 = Just (a2, c2)
plusPair (_, c1) (a2, c2) | isZeroArithmeticExpr c1 = Just (a2, c2)
plusPair (a1, c1) (a2, c2) =
  let (k1, c1') = splitConstantFactorArithmeticExpr c1
      (k2, c2') = splitConstantFactorArithmeticExpr c2
      a1' = multArithmeticExpr (integerArithmeticExpr k1) a1
      a2' = multArithmeticExpr (integerArithmeticExpr k2) a2
   in if c1' == c2'
        then Just (plusArithmeticExpr a1' a2', c1')
        else Nothing

sumPairs :: [(ArithmeticExpr, ArithmeticExpr)] -> Maybe (ArithmeticExpr, ArithmeticExpr)
sumPairs = foldr (\e1 e2 -> plusPair e1 =<< e2) (Just (integerArithmeticExpr 1, integerArithmeticExpr 0))

-- | `parseLinearFunctionBody'` parses the body of a linear function which can be decomposed to convex hull trick.
-- @parseLinearFunctionBody' f i j e@ finds a 4-tuple @a, b, c, d@ where @e = a(f[j], j) c(f[< i], i) + b(f[j], j) + d(f[< i], i)@.
--
-- TODO: What is the relation between @j@ and @k@?
parseLinearFunctionBody' :: VarName -> VarName -> VarName -> Expr -> Maybe (Expr, Expr, Expr, Expr)
parseLinearFunctionBody' f i j e = result <$> go e
  where
    result (a, c, b, d) =
      let (k, a') = splitConstantFactorArithmeticExpr a
          c' = multArithmeticExpr (integerArithmeticExpr k) c
       in (formatArithmeticExpr a', formatArithmeticExpr c', formatArithmeticExpr b, formatArithmeticExpr d)
    go = \case
      Negate' e -> do
        (a, c, b, d) <- go e
        return (a, negateArithmeticExpr c, negateArithmeticExpr b, negateArithmeticExpr d)
      Plus' e1 e2 -> do
        (a1, c1, b1, d1) <- go e1
        (a2, c2, b2, d2) <- go e2
        (a, c) <- plusPair (a1, c1) (a2, c2)
        return (a, c, plusArithmeticExpr b1 b2, plusArithmeticExpr d1 d2)
      Minus' e1 e2 -> do
        (a1, c1, b1, d1) <- go e1
        (a2, c2, b2, d2) <- go e2
        (a, c) <- plusPair (a1, c1) (negateArithmeticExpr a2, c2)
        return (a, c, minusArithmeticExpr b1 b2, minusArithmeticExpr d1 d2)
      Mult' e1 e2 -> do
        (a1, c1, b1, d1) <- go e1
        (a2, c2, b2, d2) <- go e2
        (a, c) <-
          sumPairs
            [ (multArithmeticExpr a1 a2, multArithmeticExpr c1 c2),
              (multArithmeticExpr b2 a1, c1),
              (multArithmeticExpr b1 a2, c2),
              (a1, multArithmeticExpr c1 d2),
              (a2, multArithmeticExpr c2 d1),
              (b2, d1),
              (b1, d2)
            ]
        return (a, c, multArithmeticExpr b1 b2, multArithmeticExpr d1 d2)
      e
        | f `isUnusedVar` e && j `isUnusedVar` e ->
          -- NOTE: Put constants to @d@ and simplify @a, b@
          return (integerArithmeticExpr 1, integerArithmeticExpr 0, integerArithmeticExpr 0, parseArithmeticExpr e)
      e
        | f `isUnusedVar` e && i `isUnusedVar` e ->
          return (integerArithmeticExpr 1, integerArithmeticExpr 0, parseArithmeticExpr e, integerArithmeticExpr 0)
      e@(At' _ (Var f') index) | f' == f -> case unNPlusKPattern (parseArithmeticExpr index) of
        Just (i', k) | i' == i && k < 0 -> do
          return (integerArithmeticExpr 1, integerArithmeticExpr 0, integerArithmeticExpr 0, parseArithmeticExpr e)
        Just (j', 0) | j' == j -> do
          return (integerArithmeticExpr 1, integerArithmeticExpr 0, parseArithmeticExpr e, integerArithmeticExpr 0)
        _ -> Nothing
      _ -> Nothing

parseLinearFunctionBody :: MonadAlpha m => VarName -> VarName -> Integer -> Expr -> m (Maybe (Expr, Expr, Expr, Expr, Expr, Maybe Expr))
parseLinearFunctionBody f i k = runMaybeT . go
  where
    goMin e j step size = case unNPlusKPattern (parseArithmeticExpr size) of
      Just (i', k') | i' == i && k' == k -> do
        (a, b, c, d) <- hoistMaybe $ parseLinearFunctionBody' f i j step
        -- raname @j@ to @i@
        a <- lift $ substitute j (Var i) a
        c <- lift $ substitute j (Var i) c
        return (LitInt' 1, a, b, c, d, (`Minus'` d) <$> e)
      _ -> hoistMaybe Nothing
    goMax e j step size = do
      (sign, a, b, c, d, e) <- goMin e j step size
      return (Negate' sign, a, Negate' b, Negate' c, d, Negate' <$> e)
    go = \case
      Min1' _ (Map' _ _ (Lam j _ step) (Range1' size)) -> goMin Nothing j step size
      Max1' _ (Map' _ _ (Lam j _ step) (Range1' size)) -> goMax Nothing j step size
      Min1' _ (Cons' _ e (Map' _ _ (Lam j _ step) (Range1' size))) -> goMin (Just e) j step size
      Max1' _ (Cons' _ e (Map' _ _ (Lam j _ step) (Range1' size))) -> goMax (Just e) j step size
      Min1' _ (Snoc' _ (Map' _ _ (Lam j _ step) (Range1' size)) e) -> goMin (Just e) j step size
      Max1' _ (Snoc' _ (Map' _ _ (Lam j _ step) (Range1' size)) e) -> goMax (Just e) j step size
      Negate' e -> do
        (sign, a, b, c, d, e) <- go e
        return (Negate' sign, a, b, c, Negate' d, e)
      Plus' e1 e2 | isConstantTimeExpr e2 -> do
        (sign, a, b, c, d, e) <- go e1
        return (sign, a, b, c, Plus' d e2, e)
      Plus' e1 e2 | isConstantTimeExpr e1 -> do
        (sign, a, b, c, d, e) <- go e2
        return (sign, a, b, c, Plus' e1 d, e)
      Minus' e1 e2 | isConstantTimeExpr e2 -> do
        (sign, a, b, c, d, e) <- go e1
        return (sign, a, b, c, Minus' d e2, e)
      Minus' e1 e2 | isConstantTimeExpr e1 -> do
        (sign, a, b, c, d, e) <- go e2
        return (Negate' sign, a, b, c, Minus' e1 d, e)
      Mult' e1 e2 | isConstantTimeExpr e2 -> do
        (sign, a, b, c, d, e) <- go e1
        return (Mult' sign e2, a, b, c, Mult' d e2, e)
      Mult' e1 e2 | isConstantTimeExpr e1 -> do
        (sign, a, b, c, d, e) <- go e2
        return (Mult' e1 sign, a, b, c, Mult' e1 d, e)
      _ -> hoistMaybe Nothing

getLength :: Expr -> Maybe Integer
getLength = \case
  Nil' _ -> Just 0
  Cons' _ _ xs -> succ <$> getLength xs
  Snoc' _ xs _ -> succ <$> getLength xs
  _ -> Nothing

rule :: (MonadAlpha m, MonadError Error m) => RewriteRule m
rule = makeRewriteRule "Jikka.Core.Convert.ConvexHullTrick" $ \_ -> \case
  -- build (fun f -> step(f)) base n
  Build' IntTy (Lam f _ step) base n -> runMaybeT $ do
    let ts = [ConvexHullTrickTy, ListTy IntTy]
    i <- lift genVarName'
    k <- hoistMaybe $ getLength base
    step <- replaceLenF f i k step
    -- step(f) = sign() * min (cons e(f, i) (map (fun j -> a(f, j) c(f, i) + b(f, j)) (range (i + k)))) + d(f, i)
    (sign, a, c, b, d, e) <- MaybeT $ parseLinearFunctionBody f i k step
    -- Update base when k = 0. If user's program has no bugs, it uses min(cons(x, xs)) when k = 0.
    (base, n, k, c, d, e) <- case (e, k) of
      (Just e, 0) -> do
        e0 <- lift $ substitute i (LitInt' 0) e
        d0 <- lift $ substitute i (LitInt' 0) d
        let base' = Let f (ListTy IntTy) base $ Snoc' IntTy base (Plus' (Mult' sign e0) d0)
        c <- lift $ substitute i (Plus' (Var i) (LitInt' 1)) c
        d <- lift $ substitute i (Plus' (Var i) (LitInt' 1)) d
        e <- lift $ substitute i (Plus' (Var i) (LitInt' 1)) e
        return (base', Minus' n (LitInt' 1), k + 1, c, d, Just e)
      _ -> return (base, n, k, c, d, e)
    -- base' = (cht, base)
    base' <- do
      x <- lift genVarName'
      f' <- lift $ genVarName f
      i' <- lift $ genVarName i
      a <- lift $ substitute f (Var f') a
      b <- lift $ substitute f (Var f') b
      a <- lift $ substitute i (Var i') a
      b <- lift $ substitute i (Var i') b
      -- cht for base[0], ..., base[k - 1]
      let cht = Foldl' IntTy ConvexHullTrickTy (Lam2 x ConvexHullTrickTy i' IntTy (ConvexHullTrickInsert' (Var x) a b)) ConvexHullTrickInit' (Range1' (LitInt' k))
      return $
        Let f' (ListTy IntTy) base $
          uncurryApp (Tuple' ts) [cht, Var f']
    -- step' = fun (cht, f) i ->
    --     let f' = setat f index(i) value(..)
    --     in let cht' = update cht a(i) b(i)
    --     in (cht', f')
    step' <- do
      x <- lift genVarName'
      -- value(..) = (min e (min cht f[i + k] + c(i)))
      let value = Plus' (Mult' sign (maybe id (\e -> Min2' IntTy e) e (ConvexHullTrickGetMin' (Proj' ts 0 (Var x)) c))) d
      y <- lift genVarName'
      f' <- lift $ genVarName f
      a <- lift $ substitute f (Var f') a
      b <- lift $ substitute f (Var f') b
      a <- lift $ substitute i (Plus' (Var i) (LitInt' k)) a
      b <- lift $ substitute i (Plus' (Var i) (LitInt' k)) b
      return $
        Lam2 x (TupleTy ts) i IntTy $
          Let f (ListTy IntTy) (Proj' ts 1 (Var x)) $
            Let f' (ListTy IntTy) (Snoc' IntTy (Var f) value) $
              Let y ConvexHullTrickTy (ConvexHullTrickInsert' (Proj' ts 0 (Var x)) a b) $
                uncurryApp (Tuple' ts) [Var y, Var f']
    -- proj 1 (foldl step' base' (range (n - 1)))
    return $ Proj' ts 1 (Foldl' IntTy (TupleTy ts) step' base' (Range1' n))
  _ -> return Nothing

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
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
