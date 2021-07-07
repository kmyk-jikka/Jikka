{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.MatrixExponentiation
-- Description : replaces repeated applications of linear (or, affine) functions with powers of matrices. / 線形な (あるいは affine な) 関数の繰り返しの適用を行列累乗で置き換えます。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Convert.MatrixExponentiation
  ( run,
  )
where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.Matrix
import Jikka.Core.Language.ArithmeticalExpr
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Util

fromMatrix :: Matrix ArithmeticalExpr -> Expr
fromMatrix f =
  let (h, w) = matsize f
      go row = uncurryApp (Tuple' (replicate w IntTy)) (map formatArithmeticalExpr (V.toList row))
   in uncurryApp (Tuple' (replicate h (TupleTy (replicate w IntTy)))) (map go (V.toList (unMatrix f)))

runExpr :: MonadAlpha m => [(VarName, Type)] -> Expr -> m Expr
runExpr env = \case
  orig@(Lam x (TupleTy ts) e) ->
    case curryApp e of
      (Tuple' ts', es) ->
        (fromMaybe orig <$>) . runMaybeT $ do
          guard $ length ts >= 2 && all (== IntTy) ts
          guard $ length ts' >= 2 && all (== IntTy) ts'
          xs <- V.fromList <$> replicateM (length ts) (lift (genVarName x))
          let indexOfProj = \case
                (Proj' ts'' i (Var x')) | ts'' == ts && x' == x -> Just i
                _ -> Nothing
          let replaceWithVar _ e = case indexOfProj e of
                Just i -> Var (xs V.! i)
                Nothing -> e
          rows <- forM es $ \e -> MaybeT . return $ do
            let e' = mapExpr replaceWithVar env e
            guard $ x `isUnusedVar` e'
            (row, c) <- makeVectorFromArithmeticalExpr xs (parseArithmeticalExpr e')
            guard $ c == zeroSumExpr -- TODO: support affine functions
            return row
          f <- MaybeT . return $ makeMatrix (V.fromList rows)
          return $ Lam x (TupleTy ts) (MatAp' (length ts') (length ts) (fromMatrix f) (Var x))
      _ -> return orig
  e -> return e

runProgram :: MonadAlpha m => Program -> m Program
runProgram = mapExprProgramM runExpr

-- | `run` simplifies a functions from tuples of integers to tuples of integers.
-- For example, this converts the following:
--
-- > fun xs -> (xs[0] + 2 * xs[1], xs[1])
--
-- to the follwoing:
--
-- > (fun xs -> matap ((1, 2), (0, 1)) xs)
--
-- TODO: support affine functions
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.MatrixExponentiation" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
