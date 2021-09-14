{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.Eta
-- Description : does eta-reductions and makes exprs pointful. / eta 簡約を行って式を pointful にします。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.Eta
  ( run,

    -- * internal rules
    rule,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Eta
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules

rule :: (MonadAlpha m, MonadError Error m) => RewriteRule m
rule =
  let go :: (MonadAlpha m, MonadError Error m) => RewriteEnvironment -> Expr -> (Expr -> Expr) -> m (Maybe Expr)
      go env e f = (f <$>) <$> etaExpand' (typeEnv env) e
   in makeRewriteRule "eta-expansion" $ \env -> \case
        Let x t e1 e2 -> go env e1 (\e1 -> Let x t e1 e2)
        Iterate' t k f x -> go env f (\f -> Iterate' t k f x)
        Foldl' t1 t2 f init xs -> go env f (\f -> Foldl' t1 t2 f init xs)
        Scanl' t1 t2 f init xs -> go env f (\f -> Scanl' t1 t2 f init xs)
        Build' t f xs n -> go env f (\f -> Build' t f xs n)
        Map' t1 t2 f xs -> go env f (\f -> Map' t1 t2 f xs)
        Filter' t f xs -> go env f (\f -> Filter' t f xs)
        _ -> return Nothing

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- `run` does eta-reductions in some locations.
-- This aims to:

-- * simplify other rewrite-rules

-- * convert to C++

-- TODO: expand in toplevel-let too.
--
-- == Examples
--
-- Before:
--
-- > foldl (+) 0 xs
--
-- After:
--
-- > foldl (fun y x -> y + x) 0 xs
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.Eta" $ do
  precondition $ do
    lint prog
  prog <- runProgram prog
  postcondition $ do
    lint prog
  return prog
