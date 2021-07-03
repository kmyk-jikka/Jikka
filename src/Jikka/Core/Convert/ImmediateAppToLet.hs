{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Convert.ImmediateAppToLet
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.Alpha as Alpha
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Util

runExpr :: [(VarName, Type)] -> Expr -> Expr
runExpr _ = \case
  App (Lam formal body) actual -> foldr (\((x, t), e) -> Let x t e) body (zip formal actual)
  e -> e

runProgram :: Program -> Program
runProgram = mapExprProgram runExpr

-- | `run` does beta-reductions in given programs.
-- For example, this converts the following:
--
-- > (fun x -> x + x) a
--
-- to the follwoing:
--
-- > let x = a in x + x
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.ImmediateAppToLet" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- Alpha.run prog
  prog <- return $ runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
