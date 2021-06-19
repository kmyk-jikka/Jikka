{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Language.Lint where

import Control.Monad.Writer.Strict
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util

-- | `hasNoSubscriptionInLoopCounters` checks that there are no `SubscriptTrg` in loop counters of for-loops.
-- For example, the following has subscription.
--
-- > for a[0] in range(100):
-- >     pass
-- > return a[0]
--
-- NOTE: This is allowd in the standard Python.
hasNoSubscriptionInLoopCounters :: Program -> Bool
hasNoSubscriptionInLoopCounters _ = True -- TODO

-- | `hasNoNameLeakOfLoopCounters` checks that there are no leaks of loop counters of for-loops.
-- For example, the following has a leak.
--
-- > for i in range(100):
-- >     pass
-- > return i  # => 100
hasNoNameLeakOfLoopCounters :: Program -> Bool
hasNoNameLeakOfLoopCounters _ = True -- TODO

-- | `hasNoAssignToLoopCounters` checks that there are no assignments to loop counters of for-loops.
-- For example, the following has the assignment.
--
-- > for i in range(100):
-- >     i += 1
hasNoAssignToLoopCounters :: Program -> Bool
hasNoAssignToLoopCounters _ = True -- TODO

-- | `hasNoAssignToLoopIterators` checks that there are no assignments to loop iterators of for-loops.
-- For example, the following have the assignments.
--
-- > a = list(range(10))
-- > for i in a:
-- >     a[5] = i
--
-- > a = 0
-- > for i in f(a):
-- >     a += i
hasNoAssignToLoopIterators :: Program -> Bool
hasNoAssignToLoopIterators _ = True -- TODO

-- | `hasNoReturnInLoops` checks that there are no return-statements in for-loops.
-- For example, the following has such a return-statement.
--
-- > a = list(range(10))
-- > for i in a:
-- >     return True
hasNoReturnInLoops :: Program -> Bool
hasNoReturnInLoops = getAny . execWriter . mapLargeStatementM fIf fFor
  where
    fIf e body1 body2 = return [If e body1 body2]
    fFor x iter body = do
      when (any doesPossiblyReturn body) $ do
        tell $ Any True
      return [For x iter body]

-- | `hasNoMixedAssignment` checks that there are no return-statements in for-loops.
-- For example, the following is such an assignment.
--
-- > a, b[0] = list(range(10))
-- > for i in a:
-- >     return True
--
-- NOTE: this doesn't check loop counters of `For` or `ListComp`.
hasNoMixedAssignment :: Program -> Bool
hasNoMixedAssignment prog = not (any pred (listStatements prog))
  where
    pred = \case
      AugAssign x _ _ -> hasSubscriptTrg x && hasBareNameTrg x
      AnnAssign x _ _ -> hasSubscriptTrg x && hasBareNameTrg x
      _ -> False
