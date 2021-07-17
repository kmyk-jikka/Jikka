{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.CloseAll
-- Description : does reductions about @all@ and @any@, and tries to rewrite with closed-form exprs. / @all@ と @any@ についての簡約を行い、閉じた式への書き換えを目指します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.CloseAll
  ( run,

    -- * internal rules
    rule,
    reduceAll,
    reduceAny,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

reduceAll :: MonadAlpha m => RewriteRule m
reduceAll =
  let return' = return . Just
   in RewriteRule $ \_ -> \case
        -- list build functions
        All' (Nil' _) -> return' LitTrue
        All' (Cons' _ x xs) -> return' $ And' x (All' xs)
        -- list map functions
        All' (Reversed' _ xs) -> return' $ All' xs
        All' (Sorted' _ xs) -> return' $ All' xs
        All' (Filter' _ f xs) -> do
          x <- genVarName'
          return' $ All' (Map' BoolTy BoolTy (Lam x BoolTy (Implies' (App f (Var x)) (Var x))) xs)
        All' (Map' _ _ f xs) -> case f of
          Lam x _ (Not' e) -> do
            return' $ Not' (Any' (Map' BoolTy BoolTy (Lam x BoolTy e) xs))
          Lam x _ (And' e1 e2) -> do
            x1 <- genVarName x
            x2 <- genVarName x
            return' $ And' (All' (Map' BoolTy BoolTy (Lam x1 BoolTy e1) xs)) (All' (Map' BoolTy BoolTy (Lam x2 BoolTy e2) xs))
          _ -> return Nothing
        -- others
        _ -> return Nothing

reduceAny :: MonadAlpha m => RewriteRule m
reduceAny =
  let return' = return . Just
   in RewriteRule $ \_ -> \case
        -- list build functions
        Any' (Nil' _) -> return' LitFalse
        Any' (Cons' _ x xs) -> return' $ Or' x (Any' xs)
        -- list map functions
        Any' (Reversed' _ xs) -> return' $ Any' xs
        Any' (Sorted' _ xs) -> return' $ Any' xs
        Any' (Filter' _ f xs) -> do
          x <- genVarName'
          return' $ Any' (Map' BoolTy BoolTy (Lam x BoolTy (And' (App f (Var x)) (Var x))) xs)
        Any' (Map' _ _ f xs) -> case f of
          Lam x _ (Not' e) -> do
            return' $ Not' (All' (Map' BoolTy BoolTy (Lam x BoolTy e) xs))
          Lam x _ (Or' e1 e2) -> do
            x1 <- genVarName x
            x2 <- genVarName x
            return' $ Or' (Any' (Map' BoolTy BoolTy (Lam x1 BoolTy e1) xs)) (Any' (Map' BoolTy BoolTy (Lam x2 BoolTy e2) xs))
          Lam x _ (Implies' e1 e2) -> do
            x1 <- genVarName x
            x2 <- genVarName x
            return' $ Or' (Any' (Map' BoolTy BoolTy (Lam x1 BoolTy (Negate' e1)) xs)) (Any' (Map' BoolTy BoolTy (Lam x2 BoolTy e2) xs))
          _ -> return Nothing
        -- others
        _ -> return Nothing

rule :: MonadAlpha m => RewriteRule m
rule =
  mconcat
    [ reduceAll,
      reduceAny
    ]

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` reduces `All` and `Any`.
--
-- == Examples
--
-- Before:
--
-- > any (filter (fun x -> x || f x) xs)
--
-- After:
--
-- > any xs || any (map f xs)
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
-- * `All` \(: \list(\bool) \to \bool\)
-- * `Any` \(: \list(\bool) \to \bool\)
--
-- === Boolean functions
--
-- * `Not` \(: \bool \to \bool\)
-- * `And` \(: \bool \to \bool \to \bool\)
-- * `Or` \(: \bool \to \bool \to \bool\)
-- * `Implies` \(: \bool \to \bool \to \bool\)
--
-- === List Build functions
--
-- * `Nil` \(: \forall \alpha. \list(\alpha)\)
-- * `Cons` \(: \forall \alpha. \alpha \to \list(\alpha) \to \list(\alpha)\)
--
-- === List Map functions
--
-- * `Map` \(: \forall \alpha \beta. (\alpha \to \beta) \to \list(\alpha) \to \list(\beta)\)
-- * `Filter` \(: \forall \alpha \beta. (\alpha \to \bool) \to \list(\alpha) \to \list(\beta)\)
-- * `Reversed` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
-- * `Sorted` \(: \forall \alpha. \list(\alpha) \to \list(\alpha)\)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.CloseAll" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
