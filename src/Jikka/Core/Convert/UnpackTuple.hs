{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.UnpackTuples
-- Description : unpacks and flattens tuples. / タプルを展開し平坦にします。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.UnpackTuple
  ( run,

    -- * internal rules
    rule,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.Alpha as Alpha
import Jikka.Core.Language.Beta
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

rule :: (MonadAlpha m, MonadError Error m) => RewriteRule m
rule =
  let return' = return . Just
   in RewriteRule $ \_ -> \case
        App (Lam x (TupleTy ts) body) e -> case curryApp e of
          (Tuple' ts', es) -> do
            when (ts /= ts') $ do
              throwInternalError "the types of tuple don't match"
            when (length ts /= length es) $ do
              throwInternalError "the sizes of tuple don't match"
            xs <- replicateM (length ts) (genVarName x)
            body' <- substitute x (uncurryApp (Tuple' ts) (map Var xs)) body
            return' $ uncurryApp (curryLam (zip xs ts) body') es
          _ -> return Nothing
        App (Tuple' [_]) (Proj' [_] 0 e) -> return' e
        Proj' ts i e -> case curryApp e of
          (Tuple' _, es) -> return' $ es !! fromInteger i
          (Lit (LitBuiltin (If _)), [e1, e2, e3]) -> return' $ If' (ts !! fromInteger i) e1 (Proj' ts i e2) (Proj' ts i e3)
          _ -> return Nothing
        Foldl' t2 (TupleTy [t1]) (Lam x1 (TupleTy [_]) (Lam x2 _ body)) e es -> do
          body' <- substitute x1 (App (Tuple' [t1]) (Var x1)) (Proj' [t1] 0 body)
          return' $ App (Tuple' [t1]) (Foldl' t2 t1 (Lam2 x1 t1 x2 t2 body') (Proj' [t1] 0 e) es)
        Scanl' t2 (TupleTy [t1]) (Lam x1 _ (Lam x2 (TupleTy [_]) body)) e es -> do
          body' <- substitute x1 (App (Tuple' [t1]) (Var x1)) (Proj' [t1] 0 body)
          let e' = Scanl' t2 t1 (Lam2 x1 t1 x2 t2 body') (Proj' [t1] 0 e) es
          y <- genVarName'
          let f = Map' t1 (TupleTy [t1]) (Lam y t1 (App (Tuple' [t1]) (Var y)))
          return' $ f e'
        Iterate' (TupleTy [t]) n (Lam x (TupleTy [_]) body) base -> do
          body' <- substitute x (App (Tuple' [t]) (Var x)) (Proj' [t] 0 body)
          return' $ uncurryApp (Tuple' [t]) [Iterate' t n (Lam x t body') (Proj' [t] 0 base)]
        _ -> return Nothing

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` removes unnecessary introductions and eliminations of tuples.
-- For example, this converts the following:
--
-- > (fun xs -> (proj0 xs) + (proj1 xs)) (tuple 2 1)
--
-- to the follwoing:
--
-- > (fun x0 x1 -> x0 + x1) 2 1
--
-- This can remove 1-tuples over higher-order functions.
-- For example, this converts the following:
--
-- > foldl (fun xs y -> tuple (proj0 xs + y) (tuple 0) [1, 2, 3]
--
-- to the follwoing:
--
-- > tuple (foldl (fun x y -> x + y) 0 [1, 2, 3])
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.UnpackTuple" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- Alpha.run prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
