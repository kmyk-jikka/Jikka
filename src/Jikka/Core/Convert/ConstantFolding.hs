{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

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
    reduceConstArithmeticExpr,
    reduceConstMaxExpr,
    reduceIdempotentBooleanExpr,
    reduceUnitBooleanExpr,
    reduceConstBooleanExpr,
    reduceUnitBitExpr,
    reduceConstBitExpr,
    reduceConstIntComparison,
    reduceUnitBooleanComparison,
  )
where

import Data.Bits
import Data.Either
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.QuasiRules
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
reduceConstArithmeticExpr :: Monad m => RewriteRule m
reduceConstArithmeticExpr =
  let return' = Just . LitInt'
   in simpleRewriteRule "reduceConstArithmeticExpr" $ \case
        Negate' (LitInt' a) -> return' $ - a
        Plus' a (LitInt' 0) -> Just a
        Plus' (LitInt' 0) b -> Just b
        Plus' (LitInt' a) (LitInt' b) -> return' $ a + b
        Minus' a (LitInt' 0) -> Just a
        Minus' (LitInt' 0) b -> Just (Negate' b)
        Minus' (LitInt' a) (LitInt' b) -> return' $ a - b
        Mult' _ (LitInt' 0) -> return' 0
        Mult' a (LitInt' 1) -> Just a
        Mult' (LitInt' 0) _ -> return' 0
        Mult' (LitInt' 1) b -> Just b
        Mult' (LitInt' a) (LitInt' b) -> return' $ a * b
        FloorDiv' a (LitInt' 1) -> Just a
        FloorDiv' (LitInt' a) (LitInt' b) -> Just . fromRight (Bottom' IntTy "division by zero") . (LitInt' <$>) $ floorDiv a b
        FloorMod' _ (LitInt' 1) -> return' 0
        FloorMod' (LitInt' a) (LitInt' b) -> Just . fromRight (Bottom' IntTy "modulo by zero") . (LitInt' <$>) $ floorMod a b
        CeilDiv' a (LitInt' 1) -> Just a
        CeilDiv' (LitInt' a) (LitInt' b) -> Just . fromRight (Bottom' IntTy "division by zero") . (LitInt' <$>) $ ceilDiv a b
        CeilMod' _ (LitInt' 1) -> return' 0
        CeilMod' (LitInt' a) (LitInt' b) -> Just . fromRight (Bottom' IntTy "modulo by zero") . (LitInt' <$>) $ ceilMod a b
        Pow' _ (LitInt' 0) -> return' 1
        Pow' a (LitInt' 1) -> Just a
        Pow' (LitInt' a) (LitInt' b) | b >= 0 && fromInteger b * log (abs (fromInteger a)) < 100 -> return' $ a ^ b
        Abs' (LitInt' a) -> return' $ abs a
        Gcd' a (LitInt' 0) -> Just a
        Gcd' _ (LitInt' 1) -> return' 1
        Gcd' (LitInt' 0) b -> Just b
        Gcd' (LitInt' 1) _ -> return' 1
        Gcd' (LitInt' a) (LitInt' b) -> return' $ gcd a b
        Lcm' _ (LitInt' 0) -> return' 0
        Lcm' a (LitInt' 1) -> Just a
        Lcm' (LitInt' 0) _ -> return' 0
        Lcm' (LitInt' 1) b -> Just b
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
reduceConstMaxExpr = simpleRewriteRule "reduceConstMaxExpr" $ \case
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
-- * `And` \(: \bool \to \bool \to \bool\)
-- * `Or` \(: \bool \to \bool \to \bool\)
-- * `Implies` \(: \bool \to \bool \to \bool\)
reduceIdempotentBooleanExpr :: Monad m => RewriteRule m
reduceIdempotentBooleanExpr =
  mconcat
    [ [r| "join/and" forall x. x && x = x|],
      [r| "join/or" forall x. x || x = x|],
      [r| "join/implies" forall x. implies (not x) x = x|],
      [r| "join/implies'" forall x. implies x (not x) = not x|]
    ]

-- |
-- == List of functions which are reduced
--
-- === Boolean functions
--
-- * `Not` \(: \bool \to \bool\)
-- * `And` \(: \bool \to \bool \to \bool\)
-- * `Or` \(: \bool \to \bool \to \bool\)
-- * `Implies` \(: \bool \to \bool \to \bool\)
reduceUnitBooleanExpr :: Monad m => RewriteRule m
reduceUnitBooleanExpr =
  mconcat
    [ [r| "not/true" not true = false|],
      [r| "not/false" not false = false|],
      [r| "and/false" forall x. false && x = false|],
      [r| "and/false'" forall x. x && false = false|],
      [r| "and/true" forall x. true && x = x|],
      [r| "and/true'" forall x. x && true = x|],
      [r| "or/false" forall x. false || x = x|],
      [r| "or/false'" forall x. x || false = x|],
      [r| "or/true" forall x. true || x = true|],
      [r| "or/true'" forall x. x || true = true|],
      [r| "implies/false" forall x. implies false x = true|],
      [r| "implies/false'" forall x. implies x false = not x|],
      [r| "implies/true" forall x. implies true x = x|],
      [r| "implies/true'" forall x. implies x true = true|]
    ]

-- |
-- == List of functions which are reduced
--
-- === Boolean functions
--
-- * `If` \(: \forall \alpha. \bool \to \alpha \to \alpha \to \alpha\)
reduceConstBooleanExpr :: Monad m => RewriteRule m
reduceConstBooleanExpr =
  mconcat
    [ [r| "if/true" forall e1 e2. if true then e1 else e2 = e1|],
      [r| "if/false" forall e1 e2. if false then e1 else e2 = e2|]
    ]

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
reduceUnitBitExpr :: Monad m => RewriteRule m
reduceUnitBitExpr =
  mconcat
    [ [r| "bitand/0" forall x. 0 & x = 0 |],
      [r| "bitand/0'" forall x. x & 0 = 0 |],
      [r| "bitand/-1" forall x. (-1) & x = x |],
      [r| "bitand/-1'" forall x. x & (-1) = x |],
      [r| "bitor/0" forall x. 0 | x = x |],
      [r| "bitor/0'" forall x. x | 0 = x |],
      [r| "bitor/-1" forall x. (-1) | x = -1 |],
      [r| "bitor/-1'" forall x. x | (-1) = -1 |],
      [r| "bitxor/0" forall x. 0 ^ x = x |],
      [r| "bitxor/0'" forall x. x ^ 0 = x |],
      [r| "bitxor/-1" forall x. (-1) ^ x = ~ x |],
      [r| "bitxor/-1'" forall x. x ^ (-1) = ~ x |],
      [r| "bitleftshift/0" forall x. 0 << x = 0 |],
      [r| "bitleftshift/0'" forall x. x << 0 = x |],
      [r| "bitrightshift/0" forall x. 0 >> x = 0 |],
      [r| "bitrightshift/0'" forall x. x >> 0 = x |]
    ]

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
  let return' = Just . LitInt'
   in simpleRewriteRule "reduceConstBitExpr" $ \case
        BitNot' (LitInt' a) -> return' $ complement a
        BitAnd' (LitInt' a) (LitInt' b) -> return' $ a .&. b
        BitOr' (LitInt' a) (LitInt' b) -> return' $ a .|. b
        BitXor' (LitInt' a) (LitInt' b) -> return' $ a `xor` b
        BitLeftShift' (LitInt' a) (LitInt' b) | - 100 < b && b < 100 -> return' $ a `shift` fromInteger b
        BitRightShift' (LitInt' a) (LitInt' b) | - 100 < b && b < 100 -> return' $ a `shift` fromInteger (- b)
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
reduceConstIntComparison :: Monad m => RewriteRule m
reduceConstIntComparison =
  simpleRewriteRule "comparison/const/int" $
    (LitBool' <$>) . \case
      LessThan' _ (LitInt' a) (LitInt' b) -> Just $ a < b
      LessEqual' _ (LitInt' a) (LitInt' b) -> Just $ a <= b
      GreaterThan' _ (LitInt' a) (LitInt' b) -> Just $ a > b
      GreaterEqual' _ (LitInt' a) (LitInt' b) -> Just $ a >= b
      Equal' _ (LitInt' a) (LitInt' b) -> Just $ a == b
      NotEqual' _ (LitInt' a) (LitInt' b) -> Just $ a /= b
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
reduceUnitBooleanComparison :: Monad m => RewriteRule m
reduceUnitBooleanComparison =
  mconcat
    [ -- TODO: implement lessthan and lessequal
      -- NOTE: We can ignore greaterthan and greaterequal because EqualitySolving swaps inequalities.
      [r| "equal/true" forall x. true == x = x |],
      [r| "equal/true'" forall x. x == true = x |],
      [r| "equal/false" forall x. false == x = not x |],
      [r| "equal/false'" forall x. x == false = not x |],
      [r| "notequal/true" forall x. true /= x = not x |],
      [r| "notequal/true'" forall x. x /= true = not x |],
      [r| "notequal/false" forall x. false /= x = x |],
      [r| "notequal/false'" forall x. x /= false = x |]
    ]

rule :: MonadError Error m => RewriteRule m
rule =
  mconcat
    [ reduceConstArithmeticExpr,
      reduceConstMaxExpr,
      reduceIdempotentBooleanExpr,
      reduceUnitBooleanExpr,
      reduceConstBooleanExpr,
      reduceUnitBitExpr,
      reduceConstBitExpr,
      reduceConstIntComparison,
      reduceUnitBooleanComparison
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
