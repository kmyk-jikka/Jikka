{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Jikka.Core.Convert.SpecializeFoldl
-- Description : specializes @foldl@ with concrete functions like @sum@ and @product@. / @sum@ や @product@ のような具体的な関数で @foldl@ を特殊化します。
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
module Jikka.Core.Convert.SpecializeFoldl
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.ArithmeticExpr
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.ModuloExpr
import Jikka.Core.Language.RewriteRules

convertToSum :: Expr -> Maybe Expr
convertToSum = \case
  Foldl' t1 IntTy (Lam2 x2 _ x1 _ body) init xs -> do
    (a, b) <- makeAffineFunctionFromArithmeticExpr x2 (parseArithmeticExpr body)
    guard $ isOneArithmeticExpr a
    return $ Plus' init (Sum' (Map' t1 IntTy (Lam x1 t1 (formatArithmeticExpr b)) xs))
  _ -> Nothing

convertToModSum :: Expr -> Maybe Expr
convertToModSum = \case
  Foldl' t1 IntTy (Lam2 x2 _ x1 _ body) init xs -> do
    body <- parseModuloExpr body
    (a, b) <- makeAffineFunctionFromArithmeticExpr x2 (arithmeticExprFromModuloExpr body)
    guard $ isOneArithmeticExpr a

    -- `if` is required for cases like `foldl (fun y x -> y % 2) 3 xs`, which is the same to `if xs == nil then 3 else 1`.
    let wrap :: Expr -> Expr
        wrap =
          if init `isModulo` Modulo (moduloOfModuloExpr body)
            then id
            else If' IntTy (Equal' (ListTy t1) xs (Nil' t1)) init

    return . wrap $
      ModPlus' init (ModSum' (Map' t1 IntTy (Lam x1 t1 (formatArithmeticExpr b)) xs) (moduloOfModuloExpr body)) (moduloOfModuloExpr body)
  _ -> Nothing

rule :: MonadAlpha m => RewriteRule m
rule = simpleRewriteRule "Jikka.Core.Convert.SpecializeFoldl" $ \case
  (convertToSum -> Just e) -> return e
  (convertToModSum -> Just e) -> return e
  -- TODO: Replace these operators with the better implementation like sum.
  Foldl' t1 t2 (Lam2 x2 _ x1 _ body) init xs -> case body of
    -- Product
    Mult' (Var x2') e | x2' == x2 && x2 `isUnusedVar` e -> Just $ Product' (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    Mult' e (Var x2') | x2' == x2 && x2 `isUnusedVar` e -> Just $ Product' (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    -- All
    And' (Var x2') e | x2' == x2 && x2 `isUnusedVar` e -> Just $ All' (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    And' e (Var x2') | x2' == x2 && x2 `isUnusedVar` e -> Just $ All' (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    -- Any
    Or' (Var x2') e | x2' == x2 && x2 `isUnusedVar` e -> Just $ Any' (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    Or' e (Var x2') | x2' == x2 && x2 `isUnusedVar` e -> Just $ Any' (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    -- Max1
    Max2' _ (Var x2') e | x2' == x2 && x2 `isUnusedVar` e -> Just $ Max1' t2 (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    Max2' _ e (Var x2') | x2' == x2 && x2 `isUnusedVar` e -> Just $ Max1' t2 (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    -- Max1
    Min2' _ (Var x2') e | x2' == x2 && x2 `isUnusedVar` e -> Just $ Min1' t2 (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    Min2' _ e (Var x2') | x2' == x2 && x2 `isUnusedVar` e -> Just $ Min1' t2 (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    -- Lcm1
    Lcm' (Var x2') e | x2' == x2 && x2 `isUnusedVar` e -> Just $ Lcm1' t2 (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    Lcm' e (Var x2') | x2' == x2 && x2 `isUnusedVar` e -> Just $ Lcm1' t2 (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    -- Gcd1
    Gcd' (Var x2') e | x2' == x2 && x2 `isUnusedVar` e -> Just $ Gcd1' t2 (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    Gcd' e (Var x2') | x2' == x2 && x2 `isUnusedVar` e -> Just $ Gcd1' t2 (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs))
    -- others
    _ -> Nothing
  -- The outer floor-mod is required because foldl for empty lists returns values without modulo.
  FloorMod' (Foldl' t1 t2 (Lam2 x2 _ x1 _ body) init xs) m -> case body of
    -- ModProduct
    ModMult' (Var x2') e m' | x2' == x2 && x2 `isUnusedVar` e && m' == m -> Just $ ModProduct' (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs)) m
    ModMult' e (Var x2') m' | x2' == x2 && x2 `isUnusedVar` e && m' == m -> Just $ ModProduct' (Cons' t2 init (Map' t1 t2 (Lam x1 t1 e) xs)) m
    -- others
    _ -> Nothing
  -- others
  _ -> Nothing

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` reduces summations and products.
--
-- == Example
--
-- Before:
--
-- > foldl (fun x y -> x + y) 0 xs
--
-- After:
--
-- > sum xs
--
-- == List of builtin functions which are reduced
--
-- === Source functions
--
-- * `Foldl` \(: \forall \alpha \beta. (\beta \to \alpha \to \beta) \to \beta \to \list(\alpha) \to \beta\)
--
-- === Destination functions
--
-- * `Sum` \(: \list(\int) \to \int\)
-- * `Product` \(: \list(\int) \to \int\)
-- * `ModSum` \(: \list(\int) \to \int \to \int\)
-- * `ModProduct` \(: \list(\int) \to \int \to \int\)
-- * `All` \(: \list(\bool) \to \bool\)
-- * `Any` \(: \list(\bool) \to \bool\)
-- * `Max1` \(: \forall \alpha. \list(\alpha) \to \alpha\)
-- * `Min1` \(: \forall \alpha. \list(\alpha) \to \alpha\)
-- * `Iterate` \(: \forall \alpha. \int \to (\alpha \to \alpha) \to \alpha \to \alpha\)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.SpecializeFoldl" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
