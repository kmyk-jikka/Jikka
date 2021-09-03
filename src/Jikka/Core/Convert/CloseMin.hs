{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

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
import qualified Jikka.Core.Convert.Alpha as Alpha
import Jikka.Core.Language.AssertedHint
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.LambdaPatterns
import Jikka.Core.Language.Lint
import Jikka.Core.Language.QuasiRules
import Jikka.Core.Language.RewriteRules

reduceMin :: MonadAlpha m => RewriteRule m
reduceMin =
  mconcat
    [ -- reduce minimum-cons if non-nil
      pureRewriteRule "minimum/cons" $ \env -> \case
        Min1' t (Cons' _ x xs) | nullWithHints (assertedHints env) xs == Just False -> Just $ Min2' t x (Min1' t xs)
        _ -> Nothing,
      -- list build functions
      [r| "minimum/nil" minimum nil = bottom<"no minimum in empty list"> |],
      [r| "minimum/cons/cons" forall x y zs. minimum (cons x (cons y zs)) = min x (minimum (cons y zs)) |],
      [r| "minimum/range" forall n. minimum (range n) = 0 |],
      -- list map functions
      [r| "minimum/reversed" forall xs. minimum (reversed xs) = minimum xs |],
      [r| "minimum/cons/reversed" forall x xs. minimum (cons x (reversed xs)) = minimum (cons x xs) |],
      [r| "minimum/sorted" forall xs. minimum (sorted xs) = minimum xs |],
      [r| "minimum/cons/sorted" forall x xs. minimum (cons x (sorted xs)) = minimum (cons x xs) |],
      makeRewriteRule "minimum/map/const" $ \_ -> \case
        Min1' _ (Map' _ _ (LamConst _ e) _) -> return $ Just e
        _ -> return Nothing,
      [r| "minimum/map/min" forall e1 e2 xs. minimum (map (fun x -> min e1 e2) xs) = min (minimum (map (fun x -> e1) xs)) (minimum (map (fun x -> e2) xs)) |],
      [r| "minimum/map/negate" forall e xs. minimum (map (fun x -> - e) xs) = - (maximum (map (fun x -> e) xs)) |],
      makeRewriteRule "minimum/map/plus" $ \_ -> \case
        Min1' _ (Map' t1 _ (Lam x _ (Plus' k e)) xs) | x `isUnusedVar` k -> return . Just $ Plus' k (Min1' IntTy (Map' t1 IntTy (Lam x t1 e) xs))
        _ -> return Nothing,
      makeRewriteRule "minimum/map/plus'" $ \_ -> \case
        Min1' _ (Map' t1 _ (Lam x _ (Plus' e k)) xs) | x `isUnusedVar` k -> return . Just $ Plus' k (Min1' IntTy (Map' t1 IntTy (Lam x t1 e) xs))
        _ -> return Nothing,
      makeRewriteRule "minimum/map/minus" $ \_ -> \case
        Min1' _ (Map' t1 _ (Lam x _ (Minus' k e)) xs) | x `isUnusedVar` k -> return . Just $ Minus' k (Max1' IntTy (Map' t1 IntTy (Lam x t1 e) xs))
        _ -> return Nothing,
      makeRewriteRule "minimum/map/minus'" $ \_ -> \case
        Min1' _ (Map' t1 _ (Lam x _ (Minus' e k)) xs) | x `isUnusedVar` k -> return . Just $ Minus' k (Min1' IntTy (Map' t1 IntTy (Lam x t1 e) xs))
        _ -> return Nothing,
      makeRewriteRule "minimum/cons/map/const" $ \_ -> \case
        Min1' t (Cons' _ x (Map' _ _ (LamConst _ e) _)) -> return . Just $ Min2' t x e
        _ -> return Nothing,
      [r| "minimum/cons/map/min" forall e0 e1 e2 xs. minimum (cons e0 (map (fun x -> min e1 e2) xs)) = min (minimum (cons e0 (map (fun x -> e1) xs))) (minimum (cons x (map (fun x -> e2) xs))) |],
      [r| "minimum/cons/map/negate" forall e0 e xs. minimum (cons e0 (map (fun x -> - e) xs)) = - (maximum (cons (- e0) (map (fun x -> e) xs))) |],
      makeRewriteRule "minimum/cons/map/plus" $ \_ -> \case
        Min1' _ (Cons' _ e0 (Map' t1 _ (Lam x _ (Plus' k e)) xs)) | x `isUnusedVar` k -> return . Just $ Plus' k (Min1' IntTy (Cons' IntTy (Minus' e0 k) (Map' t1 IntTy (Lam x t1 e) xs)))
        _ -> return Nothing,
      makeRewriteRule "minimum/cons/map/plus'" $ \_ -> \case
        Min1' _ (Cons' _ e0 (Map' t1 _ (Lam x _ (Plus' e k)) xs)) | x `isUnusedVar` k -> return . Just $ Plus' k (Min1' IntTy (Cons' IntTy (Minus' e0 k) (Map' t1 IntTy (Lam x t1 e) xs)))
        _ -> return Nothing,
      makeRewriteRule "minimum/cons/map/minus" $ \_ -> \case
        Min1' _ (Cons' _ e0 (Map' t1 _ (Lam x _ (Minus' k e)) xs)) | x `isUnusedVar` k -> return . Just $ Minus' k (Max1' IntTy (Cons' IntTy (Minus' k e0) (Map' t1 IntTy (Lam x t1 e) xs)))
        _ -> return Nothing,
      makeRewriteRule "minimum/cons/map/minus'" $ \_ -> \case
        Min1' _ (Cons' _ e0 (Map' t1 _ (Lam x _ (Minus' e k)) xs)) | x `isUnusedVar` k -> return . Just $ Minus' (Min1' IntTy (Cons' IntTy (Plus' e0 k) (Map' t1 IntTy (Lam x t1 e) xs))) k
        _ -> return Nothing
    ]

reduceMax :: MonadAlpha m => RewriteRule m
reduceMax =
  mconcat
    [ -- reduce maximum-cons if non-nil
      pureRewriteRule "maximum/cons" $ \env -> \case
        Max1' t (Cons' _ x xs) | nullWithHints (assertedHints env) xs == Just False -> Just $ Max2' t x (Max1' t xs)
        _ -> Nothing,
      -- list build functions
      [r| "maximum/nil" maximum nil = bottom<"no maximum in empty list"> |],
      [r| "maximum/cons/cons" forall x y zs. maximum (cons x (cons y zs)) = max x (maximum (cons y zs)) |],
      [r| "maximum/range" forall n. maximum (range n) = n - 1 |],
      -- list map functions
      [r| "maximum/reversed" forall xs. maximum (reversed xs) = maximum xs |],
      [r| "maximum/cons/reversed" forall x xs. maximum (cons x (reversed xs)) = maximum (cons x xs) |],
      [r| "maximum/sorted" forall xs. maximum (sorted xs) = maximum xs |],
      [r| "maximum/cons/sorted" forall x xs. maximum (cons x (sorted xs)) = maximum (cons x xs) |],
      makeRewriteRule "maximum/map/const" $ \_ -> \case
        Max1' _ (Map' _ _ (LamConst _ e) _) -> return $ Just e
        _ -> return Nothing,
      [r| "maximum/map/max" forall e1 e2 xs. maximum (map (fun x -> max e1 e2) xs) = max (maximum (map (fun x -> e1) xs)) (maximum (map (fun x -> e2) xs)) |],
      [r| "maximum/map/negate" forall e xs. maximum (map (fun x -> - e) xs) = - (maximum (map (fun x -> e) xs)) |],
      makeRewriteRule "maximum/map/plus" $ \_ -> \case
        Max1' _ (Map' t1 _ (Lam x _ (Plus' k e)) xs) | x `isUnusedVar` k -> return . Just $ Plus' k (Max1' IntTy (Map' t1 IntTy (Lam x t1 e) xs))
        _ -> return Nothing,
      makeRewriteRule "maximum/map/plus'" $ \_ -> \case
        Max1' _ (Map' t1 _ (Lam x _ (Plus' e k)) xs) | x `isUnusedVar` k -> return . Just $ Plus' k (Max1' IntTy (Map' t1 IntTy (Lam x t1 e) xs))
        _ -> return Nothing,
      makeRewriteRule "maximum/map/minus" $ \_ -> \case
        Max1' _ (Map' t1 _ (Lam x _ (Minus' k e)) xs) | x `isUnusedVar` k -> return . Just $ Minus' k (Min1' IntTy (Map' t1 IntTy (Lam x t1 e) xs))
        _ -> return Nothing,
      makeRewriteRule "maximum/map/minus'" $ \_ -> \case
        Max1' _ (Map' t1 _ (Lam x _ (Minus' e k)) xs) | x `isUnusedVar` k -> return . Just $ Minus' k (Max1' IntTy (Map' t1 IntTy (Lam x t1 e) xs))
        _ -> return Nothing,
      makeRewriteRule "maximum/cons/map/const" $ \_ -> \case
        Max1' t (Cons' _ x (Map' _ _ (LamConst _ e) _)) -> return . Just $ Max2' t x e
        _ -> return Nothing,
      [r| "maximum/cons/map/max" forall e0 e1 e2 xs. maximum (cons e0 (map (fun x -> max e1 e2) xs)) = max (maximum (cons e0 (map (fun x -> e1) xs))) (maximum (cons e0 (map (fun x -> e2) xs))) |],
      [r| "maximum/cons/map/negate" forall e0 e xs. maximum (cons e0 (map (fun x -> - e) xs)) = - (minimum (cons e0 (map (fun x -> e) xs))) |],
      makeRewriteRule "maximum/cons/map/plus" $ \_ -> \case
        Max1' _ (Cons' _ e0 (Map' t1 _ (Lam x _ (Plus' k e)) xs)) | x `isUnusedVar` k -> return . Just $ Plus' k (Max1' IntTy (Cons' IntTy (Minus' e0 k) (Map' t1 IntTy (Lam x t1 e) xs)))
        _ -> return Nothing,
      makeRewriteRule "maximum/cons/map/plus'" $ \_ -> \case
        Max1' _ (Cons' _ e0 (Map' t1 _ (Lam x _ (Plus' e k)) xs)) | x `isUnusedVar` k -> return . Just $ Plus' k (Max1' IntTy (Cons' IntTy (Minus' e0 k) (Map' t1 IntTy (Lam x t1 e) xs)))
        _ -> return Nothing,
      makeRewriteRule "maximum/cons/map/minus" $ \_ -> \case
        Max1' _ (Cons' _ e0 (Map' t1 _ (Lam x _ (Minus' k e)) xs)) | x `isUnusedVar` k -> return . Just $ Minus' k (Min1' IntTy (Cons' IntTy (Minus' k e0) (Map' t1 IntTy (Lam x t1 e) xs)))
        _ -> return Nothing,
      makeRewriteRule "maximum/cons/map/minus'" $ \_ -> \case
        Max1' _ (Cons' _ e0 (Map' t1 _ (Lam x _ (Minus' e k)) xs)) | x `isUnusedVar` k -> return . Just $ Minus' (Max1' IntTy (Cons' IntTy (Plus' e0 k) (Map' t1 IntTy (Lam x t1 e) xs))) k
        _ -> return Nothing
    ]

-- | TODO: implement this
reduceArgMin :: Monad m => RewriteRule m
reduceArgMin = simpleRewriteRule "reduceArgMin" $ \case
  -- list map functions
  ArgMin' t (Reversed' _ xs) -> Just $ Minus' (Minus' (Len' t xs) (ArgMin' t xs)) Lit1
  ArgMin' _ (Map' _ _ (Lam x _ e) _) | x `isUnusedVar` e -> Just Lit0
  ArgMin' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ ArgMin' t2 (Map' t1 t2 (Lam x t e2) xs)
  ArgMin' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ ArgMin' t2 (Map' t1 t2 (Lam x t e1) xs)
  _ -> Nothing

-- | TODO: implement this
reduceArgMax :: Monad m => RewriteRule m
reduceArgMax = simpleRewriteRule "reduceArgMax" $ \case
  -- list map functions
  ArgMax' t (Reversed' _ xs) -> Just $ Minus' (Minus' (Len' t xs) (ArgMax' t xs)) Lit1
  ArgMax' _ (Map' _ _ (Lam x t e) xs) | x `isUnusedVar` e -> Just $ Minus' (Len' t xs) Lit1
  ArgMax' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ ArgMax' t2 (Map' t1 t2 (Lam x t e2) xs)
  ArgMax' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ ArgMax' t2 (Map' t1 t2 (Lam x t e1) xs)
  _ -> Nothing

rule :: MonadAlpha m => RewriteRule m
rule =
  mconcat
    [ reduceMin,
      reduceMax,
      reduceArgMin,
      reduceArgMax
    ]

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
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
    lint prog
  prog <- runProgram prog
  prog <- Alpha.run prog
  postcondition $ do
    lint prog
  return prog
