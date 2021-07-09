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
    reduceFoldlSetAt,
    getRecurrenceFormulaBase,
    getRecurrenceFormulaStep1,
    getRecurrenceFormulaStep,
  )
where

import Control.Monad.Trans.Maybe
import Data.Maybe
import qualified Data.Vector as V
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.ArithmeticalExpr
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
reduceScanlBuild = simpleRewriteRule $ \case
  Scanl' _ t2 _ init (Nil' _) -> Just $ Cons' t2 init (Nil' t2)
  Scanl' t1 t2 f init (Cons' _ x xs) -> Just $ Cons' t2 init (Scanl' t1 t2 f (App2 f init x) xs)
  _ -> Nothing

-- | `getRecurrenceFormulaBase` makes a pair @((a_0, ..., a_{k - 1}), a)@ from @setat (... (setat a 0 a_0) ...) (k - 1) a_{k - 1})@.
getRecurrenceFormulaBase :: Expr -> ([Expr], Expr)
getRecurrenceFormulaBase = go (V.replicate recurrenceLimit Nothing)
  where
    recurrenceLimit :: Num a => a
    recurrenceLimit = 20
    go :: V.Vector (Maybe Expr) -> Expr -> ([Expr], Expr)
    go base = \case
      SetAt' _ e (LitInt' i) e' | 0 <= i && i < recurrenceLimit -> go (base V.// [(fromInteger i, Just e')]) e
      e -> (map fromJust (takeWhile isJust (V.toList base)), e)

-- | `getRecurrenceFormulaStep1` removes `At` in @a@.
getRecurrenceFormulaStep1 :: MonadAlpha m => Int -> Type -> VarName -> VarName -> Expr -> m (Maybe Expr)
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
  return $ case go body of
    Just body -> Just $ Lam2 x t i IntTy body
    Nothing -> Nothing

-- | `getRecurrenceFormulaStep` replaces `At` in @a@ with `Proj`.
getRecurrenceFormulaStep :: MonadAlpha m => Int -> Int -> Type -> VarName -> VarName -> Expr -> m (Maybe Expr)
getRecurrenceFormulaStep shift size t a i body = do
  x <- genVarName a
  let ts = replicate size t
  let proj k =
        if 0 <= toInteger shift + k && toInteger shift + k < toInteger size
          then Just $ Proj' ts (shift + fromInteger k) (Var x)
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
  return $ case go body of
    Just body -> Just $ Lam2 x (TupleTy ts) i IntTy (uncurryApp (Tuple' ts) (map (\i -> Proj' ts i (Var x)) [1 .. size - 1] ++ [body]))
    Nothing -> Nothing

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

-- |
-- * This assumes that `Range2` and `Range3` are already converted to `Range1` (`Jikka.Core.Convert.ShortCutFusion`).
-- * This assumes that combinations `Foldl` and `Map` squashed (`Jikka.Core.Convert.ShortCutFusion`).
-- * This assumes that constants are already folded (`Jikka.Core.Convert.ConstantFolding`).
reduceFoldlSetAt :: MonadAlpha m => RewriteRule m
reduceFoldlSetAt = RewriteRule $ \_ -> \case
  -- foldl (fun a i -> setat a index(i) step(a, i)) init indices
  Foldl' _ (ListTy t2) (Lam2 a _ i _ (SetAt' _ (Var a') index step)) init' indices | a' == a && a `isUnusedVar` index -> runMaybeT $ do
    index <- hoistMaybe $ case unNPlusKPattern (parseArithmeticalExpr index) of
      Just (i', k) | i' == i -> Just k
      _ -> Nothing
    n <- hoistMaybe $ case indices of
      Range1' n -> Just n -- We can do this because foldl-map combinations are already reduced.
      _ -> Nothing
    let (base, _) = getRecurrenceFormulaBase init'
    case base of
      [] ->
        if index == 0 && a `isUnusedVar` step
          then return $ Map' IntTy t2 (Lam i IntTy step) (Range1' n)
          else hoistMaybe Nothing
      [base] -> do
        step <- MaybeT $ getRecurrenceFormulaStep1 (- 1 + fromInteger index) t2 a i step
        return $ Scanl' IntTy t2 step base (Range1' n)
      _ -> do
        let ts = replicate (length base) t2
        let base' = uncurryApp (Tuple' ts) base
        step <- MaybeT $ getRecurrenceFormulaStep (- length base + fromInteger index) (length base) t2 a i step
        x <- lift (genVarName a)
        return $ foldr (Cons' t2) (Map' (TupleTy ts) t2 (Lam x (TupleTy ts) (Proj' ts (length base - 1) (Var x))) (Scanl' IntTy (TupleTy ts) step base' (Range1' n))) (init base)
  _ -> return Nothing

rule :: MonadAlpha m => RewriteRule m
rule =
  mconcat
    [ reduceScanlBuild,
      reduceFoldlSetAt
    ]

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` replaces `Foldl` with `Scanl`.
--
-- == List of builtin functions which are reduced
--
-- === Build functions
--
-- * `Nil` \(: \forall \alpha. \list(\alpha)\)
-- * `Cons` \(: \forall \alpha. \alpha \to \list(\alpha) \to \list(\alpha)\)
-- * `Range1` \(: \int \to \list(\int)\)
--
-- === Map functions
--
-- * `Scanl` \(: \forall \alpha \beta. (\beta \to \alpha \to \beta) \to \beta \to \list(\alpha) \to \list(\beta)\)
-- * `SetAt` \(: \forall \alpha. \list(\alpha) \to \int \to \alpha \to \list(\alpha)\)
--
-- === Fold functions
--
-- * `At` \(: \forall \alpha. \list(\alpha) \to \int \to \alpha\)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.MakeScanl" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
