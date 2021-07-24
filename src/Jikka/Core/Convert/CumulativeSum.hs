{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.CumulativeSum
-- Description : processes queries like range sum query using cumulative sums. / 累積和を用いて range sum query のようなクエリを処理します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.CumulativeSum
  ( run,

    -- * internal rules
    rule,
  )
where

import Data.Maybe
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.Alpha as Alpha
import Jikka.Core.Language.ArithmeticalExpr
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

cumulativeMax :: MonadAlpha m => (Expr -> Expr -> Expr) -> Type -> Maybe Expr -> Expr -> Expr -> m Expr
cumulativeMax max2 t a0 a n = do
  b <- genVarName'
  let e = At' t (Var b) n
  x1 <- genVarName'
  x2 <- genVarName'
  let a0' = fromMaybe (At' t a (LitInt' 0)) a0
  return $ Let b (ListTy t) (Scanl' IntTy t (Lam2 x1 t x2 t (max2 (Var x1) (Var x2))) a0' a) e

rule :: MonadAlpha m => RewriteRule m
rule = RewriteRule $ \_ -> \case
  Sum' (Map' _ _ (Lam x _ (At' _ a index)) (Range1' n)) | x `isUnusedVar` a -> do
    case makeAffineFunctionFromArithmeticalExpr x (parseArithmeticalExpr index) of
      Just (coeff, shift) | isOneArithmeticalExpr coeff -> do
        b <- genVarName'
        let e =
              if isZeroArithmeticalExpr shift
                then At' IntTy (Var b) n
                else Minus' (At' IntTy (Var b) (Plus' n (formatArithmeticalExpr shift))) (At' IntTy (Var b) (formatArithmeticalExpr shift))
        return . Just $
          Let b (ListTy IntTy) (Scanl' IntTy IntTy (Lit (LitBuiltin Plus)) Lit0 a) e
      _ -> return Nothing
  Max1' t (Cons' _ a0 (Map' _ _ (Lam x _ (At' _ a (Var x'))) (Range1' n))) | x' == x && x `isUnusedVar` a -> do
    Just <$> cumulativeMax (Max2' t) t (Just a0) a n
  Max1' t (Map' _ _ (Lam x _ (At' _ a (Var x'))) (Range1' n)) | x' == x && x `isUnusedVar` a -> do
    Just <$> cumulativeMax (Max2' t) t Nothing a n
  Min1' t (Cons' _ a0 (Map' _ _ (Lam x _ (At' _ a (Var x'))) (Range1' n))) | x' == x && x `isUnusedVar` a -> do
    Just <$> cumulativeMax (Min2' t) t (Just a0) a n
  Min1' t (Map' _ _ (Lam x _ (At' _ a (Var x'))) (Range1' n)) | x' == x && x `isUnusedVar` a -> do
    Just <$> cumulativeMax (Min2' t) t Nothing a n
  _ -> return Nothing

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` introduces cumulative sums.
--
-- == Examples
--
-- Before:
--
-- > sum (fun i -> a[i]) (range n)
--
-- After:
--
-- > let b = scanl (+) 0 a in b[n]
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.CumulativeSum" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- Alpha.run prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
