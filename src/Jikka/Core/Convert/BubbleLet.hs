{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.BubbleLet
-- Description : makes let-exprs rise in higher-order functions. / 高階関数中の let 式を浮き上がらせます。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.BubbleLet
  ( run,

    -- * internal rules
    rule,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules

rule :: MonadAlpha m => RewriteRule m
rule =
  let go f cont = case f of
        Lam x t (Let y t' e body) | x `isUnusedVar` e -> return . Just $ Let y t' e (cont (Lam x t body))
        _ -> return Nothing
   in makeRewriteRule "Jikka.Core.Convert.BubbleLet" $ \_ -> \case
        Iterate' t k f x -> go f (\f -> Iterate' t k f x)
        Foldl' t1 t2 f init xs -> go f (\f -> Foldl' t1 t2 f init xs)
        Build' t f xs n -> go f (\f -> Build' t f xs n)
        Map' t1 t2 f xs -> go f (\f -> Map' t1 t2 f xs)
        Filter' t f xs -> go f (\f -> Filter' t f xs)
        _ -> return Nothing

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` moves let-exprs in lambdas passed to higher-order functions to the outer of the higher-order functions.
--
-- == Examples
--
-- Before:
--
-- > map (fun x -> let c = 12345 in c * x) xs
--
-- After:
--
-- > let c = 12345 in map (fun x -> c * x) xs
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.BubbleLet" $ do
  precondition $ do
    lint prog
  prog <- runProgram prog
  postcondition $ do
    lint prog
  return prog
