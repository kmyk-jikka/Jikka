{-# LANGUAGE FlexibleContexts #-}

module Jikka.RestrictedPython.Convert.SplitLoops
  ( run,
    run',
    runForLoop,
  )
where

import Data.List (partition)
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.RestrictedPython.Convert.Alpha as Alpha (run)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Lint
import Jikka.RestrictedPython.Language.Util
import Jikka.RestrictedPython.Language.VariableAnalysis

-- | `runForLoop` splits a for-loop to many for-loops as possible.
-- This assumes that `doesntHaveSubscriptionInLoopCounters`, `doesntHaveAssignmentToLoopCounters`, and `doesntHaveAssignmentToLoopIterators` hold.
--
-- This function analyzes read-variables and write-variables in statements, and split statements into connected components.
runForLoop :: Target -> Expr -> [Statement] -> [Statement]
runForLoop x iter body =
  let connected (_, (r, w)) (_, (r', w')) = haveWriteReadIntersection w r' || haveWriteReadIntersection w' r
      go result [] = reverse result
      go result (stmt : stmts) =
        let (same, diff) = partition (connected stmt) stmts
         in go (For x iter (map fst (stmt : same)) : result) diff
      body' = map (\stmt -> (stmt, analyzeStatement stmt)) body
   in go [] body'

-- | `run'` splits for-loops into many small for-loops as possible.
-- This assumes that `doesntHaveSubscriptionInLoopCounters`, `doesntHaveAssignmentToLoopCounters`, and `doesntHaveAssignmentToLoopIterators` hold.
-- This may introduce name conflicts.
--
-- For example, the following
--
-- > a = 0
-- > b = 0
-- > for i in range(10):
-- >     c = b
-- >     a += i
-- >     b += c
--
-- is split to
--
-- > a = 0
-- > b = 0
-- > for i in range(10):
-- >     c = b
-- >     b += c
-- > for i in range(10):
-- >     a += i
run' :: Program -> Program
run' = mapLargeStatement (\e pred1 pred2 -> [If e pred1 pred2]) runForLoop

-- | `run` does alpha conversion, check assumptions, and `run'`.
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = do
  prog <- Alpha.run prog
  ensureDoesntHaveSubscriptionInLoopCounters prog
  ensureDoesntHaveAssignmentToLoopCounters prog
  ensureDoesntHaveAssignmentToLoopIterators prog
  prog <- return $ run' prog
  Alpha.run prog
