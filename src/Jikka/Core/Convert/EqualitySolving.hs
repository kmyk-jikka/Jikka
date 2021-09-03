{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Jikka.Core.Convert.EqualitySolving
-- Description : equality solving. / 等式を解きます
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : hotman78@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- \[
--     \newcommand\int{\mathbf{int}}
--     \newcommand\bool{\mathbf{bool}}
--     \newcommand\list{\mathbf{list}}
-- \]
module Jikka.Core.Convert.EqualitySolving
  ( run,
    rule,

    -- * internal rules
    moveLiteralToRight,
    convertGreaterToLess,
    reduceReflexivity,
    makeRightZero,
    reduceIntInjective,
    reduceNot,
    reduceListCtor,
    reduceListInjective,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.QuasiRules
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

-- | `moveLiteralToRight` moves literals to lhs of `(==)` or `(/=)`, using symmetricity.
moveLiteralToRight :: (MonadAlpha m, MonadError Error m) => RewriteRule m
moveLiteralToRight =
  mconcat
    [ simpleRewriteRule "equal/symmetricity/literal" $ \case
        Equal' t x y | isLiteral x && not (isLiteral y) -> Just $ Equal' t y x
        _ -> Nothing,
      simpleRewriteRule "notequal/symmetricity/literal" $ \case
        Equal' t x y | isLiteral x && not (isLiteral y) -> Just $ Equal' t y x
        _ -> Nothing
    ]

-- | `convertGreaterToLess` erases `(>)` and `(>=)`.
convertGreaterToLess :: (MonadAlpha m, MonadError Error m) => RewriteRule m
convertGreaterToLess =
  mconcat
    [ [r| "greaterthan->lessthan" forall x y. x > y = y < x |],
      [r| "greaterequal->lessequal" forall x y. x >= y = y <= x |]
    ]

-- | `reduceReflexivity` uses reflexivity.
reduceReflexivity :: (MonadAlpha m, MonadError Error m) => RewriteRule m
reduceReflexivity =
  mconcat
    [ [r| "lessthan/reflexivity" forall x. x == x = false |],
      [r| "lessequal/reflexivity" forall x. x == x = true |],
      [r| "equal/reflexivity" forall x. x == x = true |],
      [r| "notequal/reflexivity" forall x. x == x = false |]
    ]

-- | `makeRightZero` makes RHS of integer equality/inequality zero with subtracting RHS from both sides.
makeRightZero :: (MonadAlpha m, MonadError Error m) => RewriteRule m
makeRightZero =
  mconcat
    [ simpleRewriteRule "lessthan/right-zero" $ \case
        LessThan' IntTy x y | y /= LitInt' 0 -> Just $ LessThan' IntTy (Minus' x y) (LitInt' 0)
        _ -> Nothing,
      simpleRewriteRule "lessequal/right-zero" $ \case
        LessEqual' IntTy x y | y /= LitInt' 0 -> Just $ LessEqual' IntTy (Minus' x y) (LitInt' 0)
        _ -> Nothing,
      simpleRewriteRule "equal/right-zero" $ \case
        Equal' IntTy x y | y /= LitInt' 0 -> Just $ Equal' IntTy (Minus' x y) (LitInt' 0)
        _ -> Nothing,
      simpleRewriteRule "notequal/right-zero" $ \case
        NotEqual' IntTy x y | y /= LitInt' 0 -> Just $ NotEqual' IntTy (Minus' x y) (LitInt' 0)
        _ -> Nothing
    ]

-- | `reduceIntInjective` removes injective functions from equalities of integers.
reduceIntInjective :: (MonadAlpha m, MonadError Error m) => RewriteRule m
reduceIntInjective =
  mconcat
    [ [r| "equal/negate" forall x y k. - x == 0 = x == 0  |],
      [r| "equal/fact" forall x y. fact x - fact y == 0 = x == y  |],
      [r| "equal/fact'" forall x y. - fact x + fact y == 0 = x == y  |]
    ]

reduceNot :: (MonadAlpha m, MonadError Error m) => RewriteRule m
reduceNot =
  mconcat
    [ [r| "equal/not" forall x y. not x == y = x /= y |],
      [r| "equal/not'" forall x y. x == not y = x /= y |],
      [r| "notequal/not" forall x y. not x /= y = x == y |],
      [r| "notequal/not'" forall x y. x /= not y = x == y |]
    ]

reduceListCtor :: (MonadAlpha m, MonadError Error m) => RewriteRule m
reduceListCtor =
  mconcat
    [ [r| "equal/nil/nil" forall x xs. nil == nil = true |],
      [r| "equal/cons/nil" forall x xs. cons x xs == nil = false |],
      [r| "equal/nil/cons" forall x xs. nil == cons x xs = false |],
      [r| "equal/cons/cons" forall x xs y ys. cons x xs == cons y ys = x == y && xs == ys |]
    ]

reduceListInjective :: (MonadAlpha m, MonadError Error m) => RewriteRule m
reduceListInjective =
  mconcat
    [ [r| "equal/range/range" forall n1 n2. range n1 == range n2 = n1 == n2 |]
    ]

rule :: (MonadAlpha m, MonadError Error m) => RewriteRule m
rule =
  mconcat
    [ moveLiteralToRight,
      convertGreaterToLess,
      reduceReflexivity,
      makeRightZero,
      reduceIntInjective,
      reduceNot,
      reduceListCtor,
      reduceListInjective
    ]

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.EqualitySolving" $ do
  precondition $ do
    lint prog
  prog <- runProgram prog
  postcondition $ do
    lint prog
  return prog
