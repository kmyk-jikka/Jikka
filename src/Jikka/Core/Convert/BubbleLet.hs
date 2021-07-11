{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.BubbleLet
-- Description : bubbles let-exprs in higher-order functions. / 高階関数中の let 式を浮き上がらせます。
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
   in RewriteRule $ \_ -> \case
        Iterate' t k f x -> go f (\f -> Iterate' t k f x)
        Foldl' t1 t2 f init xs -> go f (\f -> Foldl' t1 t2 f init xs)
        Scanl' t1 t2 f init xs -> go f (\f -> Scanl' t1 t2 f init xs)
        Map' t1 t2 f xs -> go f (\f -> Map' t1 t2 f xs)
        Filter' t f xs -> go f (\f -> Filter' t f xs)
        _ -> return Nothing

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.BubbleLet" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
