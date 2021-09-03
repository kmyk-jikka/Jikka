{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.SortAbs
-- Description : remove abs with sorting. / sort によって abs を除去します。
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
module Jikka.Core.Convert.SortAbs
  ( run,

    -- * internal rules
    rule,
  )
where

import Control.Monad.Trans.Maybe
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.Alpha as Alpha
import Jikka.Core.Language.ArithmeticExpr
import Jikka.Core.Language.Beta
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.QuasiRules
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util

-- | @replaceAbsDelta x y z e@ replaces \(\levert x - y \rvert\) in \(e\) with \(z\).
replaceAbsDelta :: VarName -> VarName -> VarName -> Expr -> Expr
replaceAbsDelta x y z e = mapSubExpr go [] e
  where
    go _ = \case
      Abs' e | isZeroArithmeticExpr (parseArithmeticExpr (Minus' e (Minus' (Var x) (Var y)))) -> Var z
      Abs' e | isZeroArithmeticExpr (parseArithmeticExpr (Minus' e (Minus' (Var y) (Var x)))) -> Var z
      e -> e

swapTwoVars :: MonadAlpha m => VarName -> VarName -> Expr -> m Expr
swapTwoVars x y e = do
  x' <- genVarName x
  y' <- genVarName y
  e <- substitute x (Var x') e
  e <- substitute y (Var y') e
  e <- substitute x' (Var y) e
  substitute y' (Var x) e

-- | TODO: accept more functions
isSymmetric :: MonadAlpha m => VarName -> VarName -> Expr -> m Bool
isSymmetric x y f = do
  g <- swapTwoVars x y f
  return $ parseArithmeticExpr g == parseArithmeticExpr f

rule :: (MonadAlpha m, MonadError Error m) => RewriteRule m
rule = makeRewriteRule "sum/sum/abs/symmetric" $ \env -> \case
  Sum' (Map' IntTy _ (Lam x _ (Sum' (Map' _ _ (Lam y _ f) xs'))) xs) | xs' == xs -> runMaybeT $ do
    delta <- lift genVarName'
    let f' = replaceAbsDelta x y delta f
    guard $ f' /= f -- f has |x - y|
    guard =<< lift (isSymmetric x y f') -- symmetric
    ys <- lift $ genVarName'' xs
    i <- lift genVarName'
    j <- lift genVarName'
    lt <- lift $ substitute delta (Minus' (Var x) (Var y)) f'
    eq <- lift $ substitute delta (LitInt' 0) f'
    gt <- lift $ substitute delta (Minus' (Var y) (Var x)) f'
    let ctx = Let y IntTy (At' IntTy (Var ys) (Var j))
    let lt' = Sum' (Map' IntTy IntTy (Lam j IntTy (ctx lt)) (Range1' (Var i)))
    let eq' = Let j IntTy (Var i) (ctx eq)
    let gt' = Sum' (Map' IntTy IntTy (Lam j IntTy (ctx gt)) (Range2' (Plus' (Var i) (LitInt' 1)) (Len' IntTy (Var ys))))
    let e =
          Let ys (ListTy IntTy) (Sorted' IntTy xs) $
            Sum'
              ( Map'
                  IntTy
                  IntTy
                  ( Lam
                      i
                      IntTy
                      ( Let
                          x
                          IntTy
                          (At' IntTy (Var ys) (Var i))
                          (Plus' (Plus' lt' eq') gt')
                      )
                  )
                  (Range1' (Len' IntTy (Var ys)))
              )
    lift $ Alpha.runExpr (typeEnv env) e
  _ -> return Nothing

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

-- | `run` reduces \(\lvert \sum _ {a_i \in a} \sum _ {a_j \in a} f(a, a_i, a_j) \rvert\) to \(\mathbf{let}~ b = \mathrm{sort}(a) ~\mathbf{in}~ \sum \sum f'(a, a_i, a_j)\) when \(f\) contains \(\lvert a_i - a_j \rvert\) and \(f(a, a_i, a_j) = f(a, a_j, a_i)\) holds.
--
-- == Example
--
-- Before:
--
-- > sum (map (fun (a_i: int) ->
-- >     sum (map (fun (a_j: int) ->
-- >         abs (a_i - a_j)
-- >     ) a)
-- > ) a)
--
-- After:
--
-- > let b = sort a
-- > in sum (map (fun (i: int) ->
-- >     (sum (map (fun (b_j: int) ->
-- >         b_i - b_j
-- >     ) b[:i])
-- >     + 0
-- >     + sum (map (fun (b_j: int) ->
-- >         b_j - b_i
-- >     ) b[i + 1:]))
-- > ) (range (length b)))
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.SortAbs" $ do
  precondition $ do
    lint prog
  prog <- runProgram prog
  postcondition $ do
    lint prog
  return prog
