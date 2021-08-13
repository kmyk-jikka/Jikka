{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

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
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.QuasiRules
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

reduceAll :: MonadAlpha m => RewriteRule m
reduceAll =
  mconcat
    [ -- list build functions
      [r| "all/nil" all nil = true |],
      [r| "all/cons" forall x xs. all (cons x xs) = x && all xs |],
      -- list map functions
      [r| "all/reversed" forall xs. all (reversed xs) = all xs |],
      [r| "all/sorted" forall xs. all (sorted xs) = all xs |],
      [r| "all/filter" forall f xs. all (filter f xs) = all (map (fun x -> implies (f x) x) xs) |],
      [r| "all/map/not" forall e xs. all (map (fun x -> not e) xs) = not (any (map (fun x -> e) xs)) |],
      [r| "all/map/and" forall e1 e2 xs. all (map (fun x -> e1 && e2) xs) = all (map (fun x -> e1) xs) && all (map (fun x -> e2) xs) |]
    ]

reduceAny :: MonadAlpha m => RewriteRule m
reduceAny =
  mconcat
    [ -- list build functions
      [r| "any/nil" any nil = false |],
      [r| "any/cons" forall x xs. any (cons x xs) = x || any xs |],
      -- list map functions
      [r| "any/reversed" forall xs. any (reversed xs) = any xs |],
      [r| "any/sorted" forall xs. any (sorted xs) = any xs |],
      [r| "any/filter" forall f xs. any (filter f xs) = any (map (fun x -> f x && x) xs) |],
      [r| "any/map/not" forall e xs. any (map (fun x -> not e) xs) = not (all (map (fun x -> e) xs)) |],
      [r| "any/map/or" forall e1 e2 xs. any (map (fun x -> e1 || e2) xs) = any (map (fun x -> e1) xs) || any (map (fun x -> e2) xs) |],
      [r| "any/map/implies" forall e1 e2 xs. any (map (fun x -> implies e1 e2) xs) = any (map (fun x -> not e1) xs) || any (map (fun x -> e2) xs) |]
    ]

rule :: MonadAlpha m => RewriteRule m
rule =
  mconcat
    [ reduceAll,
      reduceAny
    ]

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
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
