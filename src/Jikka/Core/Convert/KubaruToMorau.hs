{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.KubaruToMorau
-- Description : converts Kubaru DP to Morau DP. / 配る DP を貰う DP に変換します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.KubaruToMorau
  ( run,

    -- * internal rules
    rule,
    runFunctionBody,
  )
where

import Control.Monad.Trans.Maybe
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.ArithmeticalExpr
import Jikka.Core.Language.Beta
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

-- | @runFunctionBody c i j step y x k@ returns @step'(y, x, i, k)@ s.t. @step(c, i, j) = step'(c[i + j + 1], c[i], i, i + j + 1)@
runFunctionBody :: (MonadAlpha m, MonadError Error m) => VarName -> VarName -> VarName -> Expr -> VarName -> VarName -> VarName -> MaybeT m Expr
runFunctionBody c i j step y x k = do
  step <- lift $ substitute j (Minus' (Minus' (Var k) (Var i)) (LitInt' 1)) step
  let go = \case
        Var x
          | x == c -> hoistMaybe Nothing
          | otherwise -> return $ Var x
        Lit lit -> return $ Lit lit
        At' _ (Var c') index | c' == c -> case () of
          () | parseArithmeticalExpr index == parseArithmeticalExpr (Var i) -> return $ Var x
          () | parseArithmeticalExpr index == parseArithmeticalExpr (Var k) -> return $ Var y
          () | otherwise -> hoistMaybe Nothing
        App e1 e2 -> App <$> go e1 <*> go e2
        Let x t e1 e2
          | x == c || x == i || x == j -> throwRuntimeError "name confliction found"
          | otherwise -> Let x t <$> go e1 <*> go e2
        Lam x t e
          | x == c || x == i || x == j -> throwRuntimeError "name confliction found"
          | otherwise -> Lam x t <$> go e
        Assert e1 e2 -> Assert <$> go e1 <*> go e2
  go step

-- | TODO: remove the assumption that the length of @a@ is equals to @n@
rule :: (MonadAlpha m, MonadError Error m) => RewriteRule m
rule = makeRewriteRule "Jikka.Core.Convert.KubaruToMorau" $ \_ -> \case
  -- foldl (fun b i -> foldl (fun c j -> setAt c index(i, j) step(c, i, j)) b (range m(i))) a (range n)
  Foldl' IntTy (ListTy t2) (Lam2 b _ i _ (Foldl' IntTy (ListTy t2') (Lam2 c _ j _ (SetAt' _ (Var c') index step)) (Var b') (Range1' m))) a (Range1' n)
    | t2' == t2 && b' == b && c == c' && b `isUnusedVar` m && b `isUnusedVar` index && b `isUnusedVar` step && c `isUnusedVar` index -> runMaybeT $ do
      -- m(i) = n - i - 1
      guard $ parseArithmeticalExpr m == parseArithmeticalExpr (Minus' (Minus' n (Var i)) (LitInt' 1))
      -- index(i, j) = i + j + 1
      guard $ parseArithmeticalExpr index == parseArithmeticalExpr (Plus' (Var i) (Plus' (Var j) (LitInt' 1)))
      x <- lift genVarName'
      y <- lift genVarName'
      k <- lift genVarName'
      -- get step'(y, x, i, k) s.t. step(c, i, j) = step'(c[i + j + 1], c[i], i, i + j + 1)
      step <- runFunctionBody c i j step y x k
      step <- lift $ substitute x (At' t2 (Var c) (Var i)) step
      step <- lift $ substitute k (Len' t2 (Var c)) step
      let base = At' t2 a (Len' t2 (Var c))
      return $ Build' t2 (Lam c (ListTy t2) (Foldl' IntTy t2 (Lam2 y t2 i IntTy step) base (Range1' (Len' t2 (Var c))))) (Nil' t2) n
  _ -> return Nothing

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` converts Kubaru DP
-- (for each \(i\), updates \(
--     \mathrm{dp}(j) \gets f(\mathrm{dp}(j), \mathrm{dp}(i))
-- \) for each \(j \gt i\))
-- to Morau DP
-- (for each \(i\), computes \(
--     \mathrm{dp}(i) = F(\lbrace \mathrm{dp}(j) \mid j \lt i \rbrace)
-- \)).
--
-- == Examples
--
-- Before:
--
-- > foldl (fun dp i ->
-- >     foldl (fun dp j ->
-- >         setAt dp j (
-- >             f dp[j] dp[i])
-- >         ) dp (range (i + 1) n)
-- >     ) dp (range n)
--
-- After:
--
-- > build (fun dp' ->
-- >     foldl (fun dp_i j ->
-- >         f dp_i dp'[j]
-- >         ) dp[i] (range i)
-- >     ) [] n
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.KubaruToMorau" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
