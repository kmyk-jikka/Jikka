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

import Data.Maybe
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

expandExpr :: MonadAlpha m => Type -> Expr -> m (Maybe Expr)
expandExpr t e = case (t, e) of
  (FunTy t1 t2, Lam x _ body) -> do
    body <- expandExpr t2 body
    return $ Lam x t1 <$> body
  (FunTy t1 t2, e) -> do
    x <- genVarName'
    let e' = App e (Var x)
    e'' <- expandExpr t2 e'
    return . Just $ Lam x t1 (fromMaybe e' e'')
  _ -> return Nothing

rule :: MonadAlpha m => RewriteRule m
rule =
  let go :: MonadAlpha m => Expr -> Type -> (Expr -> Expr) -> m (Maybe Expr)
      go e t f = (f <$>) <$> expandExpr t e
   in RewriteRule $ \_ -> \case
        Let x t e1 e2 -> go e1 t (\e1 -> Let x t e1 e2)
        Iterate' t k f x -> go f (FunTy t t) (\f -> Iterate' t k f x)
        Foldl' t1 t2 f init xs -> go f (FunTy t2 (FunTy t1 t1)) (\f -> Foldl' t1 t2 f init xs)
        Scanl' t1 t2 f init xs -> go f (FunTy t2 (FunTy t1 t1)) (\f -> Scanl' t1 t2 f init xs)
        Map' t1 t2 f xs -> go f (FunTy t1 t2) (\f -> Map' t1 t2 f xs)
        Filter' t f xs -> go f (FunTy t BoolTy) (\f -> Filter' t f xs)
        _ -> return Nothing

runProgram :: MonadAlpha m => Program -> m Program
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
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
