{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Convert.ConstantFolding
  ( run,
  )
where

import Data.Bits
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Runtime
import Jikka.Core.Language.Util

runExpr :: MonadError Error m => [(VarName, Type)] -> Expr -> m Expr
runExpr _ = \case
  Negate' (LitInt' a) -> return $ LitInt' (- a)
  Plus' (LitInt' a) (LitInt' b) -> return $ LitInt' (a + b)
  Minus' (LitInt' a) (LitInt' b) -> return $ LitInt' (a - b)
  Mult' (LitInt' a) (LitInt' b) -> return $ LitInt' (a * b)
  FloorDiv' (LitInt' a) (LitInt' b) | b /= 0 -> LitInt' <$> floorDiv a b
  FloorMod' (LitInt' a) (LitInt' b) | b /= 0 -> LitInt' <$> floorMod a b
  CeilDiv' (LitInt' a) (LitInt' b) | b /= 0 -> LitInt' <$> ceilDiv a b
  CeilMod' (LitInt' a) (LitInt' b) | b /= 0 -> LitInt' <$> ceilMod a b
  Pow' (LitInt' a) (LitInt' b) | b >= 0 && fromInteger b * log (abs (fromInteger a)) < 100 -> return $ LitInt' (a ^ b)
  Abs' (LitInt' a) -> return $ LitInt' (abs a)
  Gcd' (LitInt' a) (LitInt' b) -> return $ LitInt' (gcd a b)
  Lcm' (LitInt' a) (LitInt' b) -> return $ LitInt' (lcm a b)
  Min2' _ (LitInt' a) (LitInt' b) -> return $ LitInt' (min a b)
  Max2' _ (LitInt' a) (LitInt' b) -> return $ LitInt' (max a b)
  Not' (LitBool' a) -> return $ LitBool' (not a)
  And' (LitBool' a) (LitBool' b) -> return $ LitBool' (a && b)
  Or' (LitBool' a) (LitBool' b) -> return $ LitBool' (a || b)
  Implies' (LitBool' a) (LitBool' b) -> return $ LitBool' (not a || b)
  If' _ (LitBool' a) e1 e2 -> return $ if a then e1 else e2
  BitNot' (LitInt' a) -> return $ LitInt' (complement a)
  BitAnd' (LitInt' a) (LitInt' b) -> return $ LitInt' (a .&. b)
  BitOr' (LitInt' a) (LitInt' b) -> return $ LitInt' (a .|. b)
  BitXor' (LitInt' a) (LitInt' b) -> return $ LitInt' (a `xor` b)
  BitLeftShift' (LitInt' a) (LitInt' b) | - 100 < b && b < 100 -> return $ LitInt' (a `shift` fromInteger b)
  BitRightShift' (LitInt' a) (LitInt' b) | - 100 < b && b < 100 -> return $ LitInt' (a `shift` fromInteger (- b))
  LessThan' _ (LitInt' a) (LitInt' b) -> return $ LitBool' (a < b)
  LessEqual' _ (LitBool' a) (LitBool' b) -> return $ LitBool' (a <= b)
  LessEqual' _ (LitInt' a) (LitInt' b) -> return $ LitBool' (a <= b)
  GreaterThan' _ (LitBool' a) (LitBool' b) -> return $ LitBool' (a > b)
  GreaterThan' _ (LitInt' a) (LitInt' b) -> return $ LitBool' (a > b)
  GreaterEqual' _ (LitBool' a) (LitBool' b) -> return $ LitBool' (a >= b)
  Equal' _ (LitInt' a) (LitInt' b) -> return $ LitBool' (a == b)
  Equal' _ (LitBool' a) (LitBool' b) -> return $ LitBool' (a == b)
  NotEqual' _ (LitInt' a) (LitInt' b) -> return $ LitBool' (a /= b)
  NotEqual' _ (LitBool' a) (LitBool' b) -> return $ LitBool' (a /= b)
  e -> return e

runProgram :: MonadError Error m => Program -> m Program
runProgram = mapExprProgramM runExpr

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
