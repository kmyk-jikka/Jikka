{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.StrengthReduction
-- Description : does strength reduction. / 演算子強度低減を行います。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.StrengthReduction
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules

-- | `eliminateSomeBuiltins` removes some `Builtin` from `Expr` at all.
eliminateSomeBuiltins :: Monad m => RewriteRule m
eliminateSomeBuiltins = simpleRewriteRule $ \case
  -- arithmetical functions
  Minus' e1 e2 -> Just $ Plus' e1 (Negate' e2)
  -- advanced arithmetical functions
  Abs' e -> Just $ Max2' IntTy e (Negate' e)
  Lcm' e1 e2 -> Just $ FloorDiv' (Gcd' e1 e2) (Mult' e1 e2)
  -- logical functions
  Implies' e1 e2 -> Just $ Or' (Not' e1) e2
  -- comparison
  GreaterThan' t e1 e2 -> Just $ LessThan' t e2 e1
  GreaterEqual' t e1 e2 -> Just $ LessEqual' t e2 e1
  NotEqual' t e1 e2 -> Just $ Not' (Equal' t e1 e2)
  _ -> Nothing

-- | `reduceNegate` brings `Negate` to the root.
reduceNegate :: Monad m => RewriteRule m
reduceNegate = simpleRewriteRule $ \case
  Negate' (Negate' e) -> Just e
  Plus' (Negate' e1) (Negate' e2) -> Just $ Negate' (Plus' e1 e2)
  -- `Minus` is already removed.
  Mult' (Negate' e1) e2 -> Just $ Negate' (Mult' e1 e2)
  Mult' e1 (Negate' e2) -> Just $ Negate' (Mult' e1 e2)
  -- `Abs` is already removed.
  Min2' IntTy (Negate' e1) (Negate' e2) -> Just $ Negate' (Max2' IntTy e1 e2)
  Max2' IntTy (Negate' e1) (Negate' e2) -> Just $ Negate' (Min2' IntTy e1 e2)
  _ -> Nothing

-- | `reduceNot` brings `Not` to the root.
reduceNot :: Monad m => RewriteRule m
reduceNot = simpleRewriteRule $ \case
  Not' (Not' e) -> Just e
  And' (Not' e1) (Not' e2) -> Just $ Not' (Or' e1 e2)
  Or' (Not' e1) (Not' e2) -> Just $ Not' (And' e1 e2)
  -- `Implies` is already removed.
  Mult' (Negate' e1) e2 -> Just $ Negate' (Mult' e1 e2)
  Mult' e1 (Negate' e2) -> Just $ Negate' (Mult' e1 e2)
  If' t (Not' e1) e2 e3 -> Just $ If' t e1 e3 e2
  _ -> Nothing

-- | `reduceBitNot` brings `BitNot` to the root.
reduceBitNot :: Monad m => RewriteRule m
reduceBitNot = simpleRewriteRule $ \case
  BitNot' (BitNot' e) -> Just e
  BitAnd' (BitNot' e1) (BitNot' e2) -> Just $ BitNot' (BitOr' e1 e2)
  BitOr' (BitNot' e1) (BitNot' e2) -> Just $ BitNot' (BitAnd' e1 e2)
  BitXor' (BitNot' e1) e2 -> Just $ BitNot' (BitXor' e1 e2)
  BitXor' e1 (BitNot' e2) -> Just $ BitNot' (BitXor' e1 e2)
  _ -> Nothing

reduceAssoc :: Monad m => RewriteRule m
reduceAssoc = simpleRewriteRule $ \case
  Plus' (Plus' e1 e2) e3 -> Just $ Plus' e1 (Plus' e2 e3)
  Minus' (Minus' e1 e2) e3 -> Just $ Minus' e1 (Minus' e2 e3)
  Mult' (Mult' e1 e2) e3 -> Just $ Mult' e1 (Mult' e2 e3)
  Max2' t1 (Max2' t2 e1 e2) e3 -> Just $ Max2' t1 e1 (Max2' t2 e2 e3)
  Min2' t1 (Min2' t2 e1 e2) e3 -> Just $ Min2' t1 e1 (Min2' t2 e2 e3)
  And' (And' e1 e2) e3 -> Just $ And' e1 (And' e2 e3)
  Or' (Or' e1 e2) e3 -> Just $ Or' e1 (Or' e2 e3)
  BitAnd' (BitAnd' e1 e2) e3 -> Just $ BitAnd' e1 (BitAnd' e2 e3)
  BitOr' (BitOr' e1 e2) e3 -> Just $ BitOr' e1 (BitOr' e2 e3)
  BitXor' (BitXor' e1 e2) e3 -> Just $ BitXor' e1 (BitXor' e2 e3)
  _ -> Nothing

misc :: Monad m => RewriteRule m
misc = simpleRewriteRule $ \case
  -- arithmetical functions
  Pow' (Pow' e1 e2) e3 -> Just $ Pow' e1 (Plus' e2 e3)
  -- advanced arithmetical functions
  Gcd' (Mult' k1 e1) (Mult' k2 e2) | k1 == k2 -> Just $ Mult' k1 (Gcd' e1 e2)
  Gcd' (Mult' k1 e1) (Mult' e2 k2) | k1 == k2 -> Just $ Mult' k1 (Gcd' e1 e2)
  Gcd' (Mult' e1 k1) (Mult' e2 k2) | k1 == k2 -> Just $ Mult' k1 (Gcd' e1 e2)
  Gcd' (Mult' e1 k1) (Mult' k2 e2) | k1 == k2 -> Just $ Mult' k1 (Gcd' e1 e2)
  _ -> Nothing

rule :: MonadAlpha m => RewriteRule m
rule =
  mconcat
    [ eliminateSomeBuiltins,
      reduceNegate,
      reduceNot,
      reduceBitNot,
      reduceAssoc,
      misc
    ]

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | TODO: Split and remove this module.
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.StrengthReduction" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
