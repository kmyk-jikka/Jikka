{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Convert.UnpackTuple
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.Alpha as Alpha
import Jikka.Core.Language.Beta
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Util

runAppLam :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> [Expr] -> m Expr
runAppLam = go [] []
  where
    go :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> [Expr] -> [(VarName, Type)] -> Expr -> [Expr] -> m Expr
    go formal actual [] body [] = return $ App (Lam (reverse formal) body) (reverse actual)
    go formal' actual' ((x, TupleTy ts) : formal) body (Tuple' ts' es : actual) = do
      when (ts /= ts') $ do
        throwInternalError "the types of tuple don't match"
      when (length ts /= length es) $ do
        throwInternalError "the sizes of tuple don't match"
      xs <- replicateM (length ts) (genVarName x)
      let body' = substitute x (Tuple' ts (map Var xs)) body
      go (reverse (zip xs ts) ++ formal') (reverse es ++ actual') formal body' actual
    go formal' actual' (xt : formal) body (e : actual) = go (xt : formal') (e : actual') formal body actual
    go _ _ _ _ _ = throwInternalError "the numbers of arguments dont' match"

runExpr :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> m Expr
runExpr _ = \case
  App (Lam formal body) actual -> runAppLam formal body actual
  Proj' ts i (Tuple' ts' es) -> do
    when (ts /= ts') $ do
      throwInternalError "the types of tuple don't match"
    return $ es !! i
  Foldl' t2 (TupleTy [t1]) (Lam [(x1, TupleTy [_]), (x2, _)] (Tuple' [_] [body])) (Tuple' [_] [e]) es ->
    let body' = substitute x1 (Tuple' [t1] [Var x1]) body
     in return $ Tuple' [t1] [Foldl' t2 t1 (Lam [(x1, t1), (x2, t2)] body') e es]
  Scanl' t2 (TupleTy [t1]) (Lam [(x1, _), (x2, TupleTy [_])] (Tuple' [_] [body])) (Tuple' [_] [e]) es -> do
    let body' = substitute x1 (Tuple' [t1] [Var x1]) body
    let e' = Scanl' t2 t1 (Lam [(x1, t1), (x2, t2)] body') e es
    y <- genVarName'
    let f = Map' t1 (TupleTy [t1]) (Lam [(y, t1)] (Tuple' [t1] [Var y]))
    return $ f e'
  e -> return e

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = mapExprProgramM' runExpr runExpr

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
  prog <- Alpha.run prog
  prog <- runProgram prog
  ensureWellTyped prog
  return prog
