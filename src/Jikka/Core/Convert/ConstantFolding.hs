{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.ConstantFolding
-- Description : folds constants. / 定数畳み込みをします。
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
module Jikka.Core.Convert.ConstantFolding
  ( run,

    -- * internal rules
    rule,
    reduceConstArithmeticalExpr,
    reduceConstMaxExpr,
    reduceConstBooleanExpr,
    reduceConstBitExpr,
    reduceConstComparison,
  )
where

import Data.Bits
import Data.Either
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Runtime

-- |
-- == List of functions which are reduced
--
-- === Basic arithmetical functions
--
-- * `Negate` \(: \int \to \int\)
-- * `Plus` \(: \int \to \int \to \int\)
-- * `Minus` \(: \int \to \int \to \int\)
-- * `Mult` \(: \int \to \int \to \int\)
-- * `FloorDiv` \(: \int \to \int \to \int\)
-- * `FloorMod` \(: \int \to \int \to \int\)
-- * `CeilDiv` \(: \int \to \int \to \int\)
-- * `CeilMod` \(: \int \to \int \to \int\)
-- * `Pow` \(: \int \to \int \to \int\)
--
-- === Advanced arithmetical functions
--
-- * `Abs` \(: \int \to \int\)
-- * `Gcd` \(: \int \to \int \to \int\)
-- * `Lcm` \(: \int \to \int \to \int\)
reduceConstArithmeticalExpr :: Monad m => RewriteRule m
reduceConstArithmeticalExpr =
  let return' = Just . LitInt'
   in simpleRewriteRule $ \case
        Negate' (LitInt' a) -> return' $ - a
        Plus' (LitInt' a) (LitInt' b) -> return' $ a + b
        Minus' (LitInt' a) (LitInt' b) -> return' $ a - b
        Mult' (LitInt' a) (LitInt' b) -> return' $ a * b
        FloorDiv' (LitInt' a) (LitInt' b) -> Just . fromRight (Bottom' IntTy "division by zero") . (LitInt' <$>) $ floorDiv a b
        FloorMod' (LitInt' a) (LitInt' b) -> Just . fromRight (Bottom' IntTy "modulo by zero") . (LitInt' <$>) $ floorMod a b
        CeilDiv' (LitInt' a) (LitInt' b) -> Just . fromRight (Bottom' IntTy "division by zero") . (LitInt' <$>) $ ceilDiv a b
        CeilMod' (LitInt' a) (LitInt' b) -> Just . fromRight (Bottom' IntTy "modulo by zero") . (LitInt' <$>) $ ceilMod a b
        Pow' (LitInt' a) (LitInt' b) | b >= 0 && fromInteger b * log (abs (fromInteger a)) < 100 -> return' $ a ^ b
        Abs' (LitInt' a) -> return' $ abs a
        Gcd' (LitInt' a) (LitInt' b) -> return' $ gcd a b
        Lcm' (LitInt' a) (LitInt' b) -> return' $ lcm a b
        _ -> Nothing

-- |
-- == List of functions which are reduced
--
-- === Max functions
--
-- * `Min2` \(: \forall \alpha. \alpha \to \alpha \to \alpha\) (specialized to \(\alpha = \lbrace \bool, \int \rbrace\))
-- * `Max2` \(: \forall \alpha. \alpha \to \alpha \to \alpha\) (specialized to \(\alpha = \lbrace \bool, \int \rbrace\))
reduceConstMaxExpr :: Monad m => RewriteRule m
reduceConstMaxExpr = simpleRewriteRule $ \case
  Min2' _ (LitInt' a) (LitInt' b) -> Just . LitInt' $ min a b
  Min2' _ (LitBool' a) (LitBool' b) -> Just . LitBool' $ min a b
  Max2' _ (LitInt' a) (LitInt' b) -> Just . LitInt' $ max a b
  Max2' _ (LitBool' a) (LitBool' b) -> Just . LitBool' $ max a b
  _ -> Nothing

-- |
-- == List of functions which are reduced
--
-- === Boolean functions
--
-- * `Not` \(: \bool \to \bool\)
-- * `And` \(: \bool \to \bool \to \bool\)
-- * `Or` \(: \bool \to \bool \to \bool\)
-- * `Implies` \(: \bool \to \bool \to \bool\)
-- * `If` \(: \forall \alpha. \bool \to \alpha \to \alpha \to \alpha\)
reduceConstBooleanExpr :: Monad m => RewriteRule m
reduceConstBooleanExpr = simpleRewriteRule $ \case
  Not' (LitBool' a) -> Just $ LitBool' (not a)
  And' (LitBool' a) (LitBool' b) -> Just $ LitBool' (a && b)
  Or' (LitBool' a) (LitBool' b) -> Just $ LitBool' (a || b)
  Implies' (LitBool' a) (LitBool' b) -> Just $ LitBool' (not a || b)
  If' _ (LitBool' a) e1 e2 -> Just $ if a then e1 else e2
  _ -> Nothing

-- |
-- == List of functions which are reduced
--
-- === Bitwise boolean functions
--
-- * `BitNot` \(: \int \to \int\)
-- * `BitAnd` \(: \int \to \int \to \int\)
-- * `BitOr` \(: \int \to \int \to \int\)
-- * `BitXor` \(: \int \to \int \to \int\)
-- * `BitLeftShift` \(: \int \to \int \to \int\)
-- * `BitRightShift` \(: \int \to \int \to \int\)
reduceConstBitExpr :: Monad m => RewriteRule m
reduceConstBitExpr =
  simpleRewriteRule $
    (LitInt' <$>) . \case
      BitNot' (LitInt' a) -> Just $ complement a
      BitAnd' (LitInt' a) (LitInt' b) -> Just $ a .&. b
      BitOr' (LitInt' a) (LitInt' b) -> Just $ a .|. b
      BitXor' (LitInt' a) (LitInt' b) -> Just $ a `xor` b
      BitLeftShift' (LitInt' a) (LitInt' b) | - 100 < b && b < 100 -> Just $ a `shift` fromInteger b
      BitRightShift' (LitInt' a) (LitInt' b) | - 100 < b && b < 100 -> Just $ a `shift` fromInteger (- b)
      _ -> Nothing

-- |
-- == List of functions which are reduced
--
-- === Comparison functions
--
-- * `LessThan` \(: \forall \alpha. \alpha \to \alpha \to \bool\) (specialized to \(\alpha \in \lbrace \bool, \int \rbrace\))
-- * `LessEqual` \(: \forall \alpha. \alpha \to \alpha \to \bool\) (specialized to \(\alpha \in \lbrace \bool, \int \rbrace\))
-- * `GreaterThan` \(: \forall \alpha. \alpha \to \alpha \to \bool\) (specialized to \(\alpha \in \lbrace \bool, \int \rbrace\))
-- * `GreaterEqual` \(: \forall \alpha. \alpha \to \alpha \to \bool\) (specialized to \(\alpha \in \lbrace \bool, \int \rbrace\))
-- * `Equal` \(: \forall \alpha. \alpha \to \alpha \to \bool\) (specialized to \(\alpha \in \lbrace \bool, \int \rbrace\))
-- * `NotEqual` \(: \forall \alpha. \alpha \to \alpha \to \bool\) (specialized to \(\alpha \in \lbrace \bool, \int \rbrace\))
reduceConstComparison :: Monad m => RewriteRule m
reduceConstComparison =
  simpleRewriteRule $
    (LitBool' <$>) . \case
      LessThan' _ (LitInt' a) (LitInt' b) -> Just $ a < b
      LessEqual' _ (LitBool' a) (LitBool' b) -> Just $ a <= b
      LessEqual' _ (LitInt' a) (LitInt' b) -> Just $ a <= b
      GreaterThan' _ (LitBool' a) (LitBool' b) -> Just $ a > b
      GreaterThan' _ (LitInt' a) (LitInt' b) -> Just $ a > b
      GreaterEqual' _ (LitBool' a) (LitBool' b) -> Just $ a >= b
      Equal' _ (LitInt' a) (LitInt' b) -> Just $ a == b
      Equal' _ (LitBool' a) (LitBool' b) -> Just $ a == b
      NotEqual' _ (LitInt' a) (LitInt' b) -> Just $ a /= b
      NotEqual' _ (LitBool' a) (LitBool' b) -> Just $ a /= b
      _ -> Nothing

rule :: MonadError Error m => RewriteRule m
rule =
  mconcat
    [ reduceConstArithmeticalExpr,
      reduceConstMaxExpr,
      reduceConstBooleanExpr,
      reduceConstBitExpr,
      reduceConstComparison
    ]

runProgram :: MonadError Error m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` folds constants in given programs.
-- For example, this converts the following:
--
-- > 3 x + 2 + 1
--
-- to the follwoing:
--
-- > 3 x + 3
run :: MonadError Error m => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.ConstantFolding" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
