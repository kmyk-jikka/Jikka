{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Language.Lint where

import Control.Monad.Writer.Strict
import qualified Data.Set as S
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Builtin (builtinNames)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util
import Jikka.RestrictedPython.Language.VariableAnalysis

makeEnsureProgram :: MonadError Error m => (Program -> Bool) -> String -> Program -> m ()
makeEnsureProgram pred msg prog =
  unless (pred prog) $ do
    throwSemanticError msg

-- | `hasSubscriptionInLoopCounters` checks that there are `SubscriptTrg` in loop counters of for-loops.
-- This includes loop counters of `ListComp`.
-- For example, the followings has such subscriptions.
--
-- > for a[0] in range(100):
-- >     pass
-- > return a[0]  # => 99
--
-- > a = [0]
-- > b = [0 for a[0] in range(100)]
-- > return a[0]  # => 99
--
-- NOTE: This is allowd in the standard Python.
hasSubscriptionInLoopCounters :: Program -> Bool
hasSubscriptionInLoopCounters prog = any checkStatement (listStatements prog) || any checkExpr (listExprs prog)
  where
    checkStatement = \case
      For x _ _ -> hasSubscriptTrg x
      _ -> False
    checkExpr = \case
      ListComp _ (Comprehension x _ _) -> hasSubscriptTrg x
      _ -> False

doesntHaveSubscriptionInLoopCounters :: Program -> Bool
doesntHaveSubscriptionInLoopCounters = not . hasSubscriptionInLoopCounters

ensureDoesntHaveSubscriptionInLoopCounters :: MonadError Error m => Program -> m ()
ensureDoesntHaveSubscriptionInLoopCounters = makeEnsureProgram doesntHaveSubscriptionInLoopCounters "there must not be subscription in loop counters"

-- | `hasLeakOfLoopCounters` checks that there are leaks of loop counters of for-loops.
-- For example, the following has a leak.
--
-- > for i in range(100):
-- >     pass
-- > return i  # => 100
hasLeakOfLoopCounters :: Program -> Bool
hasLeakOfLoopCounters _ = False -- TODO

doesntHaveLeakOfLoopCounters :: Program -> Bool
doesntHaveLeakOfLoopCounters = not . hasLeakOfLoopCounters

ensureDoesntHaveLeakOfLoopCounters :: MonadError Error m => Program -> m ()
ensureDoesntHaveLeakOfLoopCounters = makeEnsureProgram doesntHaveLeakOfLoopCounters "there must not be leaks of loop counters"

-- | `hasAssignmentToLoopCounters` checks that there are assignments to loop counters of for-loops.
-- For example, the following has the assignment.
--
-- > for i in range(100):
-- >     i += 1
hasAssignmentToLoopCounters :: Program -> Bool
hasAssignmentToLoopCounters prog = any check (listStatements prog)
  where
    check = \case
      For x _ body ->
        let r = ReadList $ targetVars x
            (_, w) = analyzeStatementsMax body
         in haveWriteReadIntersection w r
      _ -> False

doesntHaveAssignmentToLoopCounters :: Program -> Bool
doesntHaveAssignmentToLoopCounters = not . hasAssignmentToLoopCounters

ensureDoesntHaveAssignmentToLoopCounters :: MonadError Error m => Program -> m ()
ensureDoesntHaveAssignmentToLoopCounters = makeEnsureProgram doesntHaveAssignmentToLoopCounters "there must not be assignments to loop counters"

-- | `hasAssignmentToLoopIterators` checks that there are assignments to loop iterators of for-loops.
-- For example, the followings have the assignments.
--
-- > a = list(range(10))
-- > for i in a:
-- >     a[5] = i
--
-- > a = 0
-- > for i in f(a):
-- >     a += i
hasAssignmentToLoopIterators :: Program -> Bool
hasAssignmentToLoopIterators prog = any check (listStatements prog)
  where
    check = \case
      For _ iter body ->
        let r = analyzeExpr iter
            (_, w) = analyzeStatementsMax body
         in haveWriteReadIntersection w r
      _ -> False

doesntHaveAssignmentToLoopIterators :: Program -> Bool
doesntHaveAssignmentToLoopIterators = not . hasAssignmentToLoopIterators

ensureDoesntHaveAssignmentToLoopIterators :: MonadError Error m => Program -> m ()
ensureDoesntHaveAssignmentToLoopIterators = makeEnsureProgram doesntHaveAssignmentToLoopIterators "there must not be assignments changing loop iterators"

-- | `hasReturnInLoops` checks that there are return-statements in for-loops.
-- For example, the following has such a return-statement.
--
-- > a = list(range(10))
-- > for i in a:
-- >     return True
hasReturnInLoops :: Program -> Bool
hasReturnInLoops = getAny . execWriter . mapLargeStatementM fIf fFor
  where
    fIf e body1 body2 = return [If e body1 body2]
    fFor x iter body = do
      when (any doesPossiblyReturn body) $ do
        tell $ Any True
      return [For x iter body]

doesntHaveReturnInLoops :: Program -> Bool
doesntHaveReturnInLoops = not . hasReturnInLoops

ensureDoesntHaveReturnInLoops :: MonadError Error m => Program -> m ()
ensureDoesntHaveReturnInLoops = makeEnsureProgram doesntHaveReturnInLoops "there must not be return-statements in for-loops"

-- | `hasMixedAssignment` checks that there are assignments which assign to both of bare variables and subscripted variables.
-- For example, the following is such an assignment.
--
-- > a, b[0] = list(range(10))
--
-- NOTE: this doesn't check loop counters of `For` or `ListComp`.
hasMixedAssignment :: Program -> Bool
hasMixedAssignment prog = any check (listStatements prog)
  where
    check = \case
      AugAssign x _ _ -> hasSubscriptTrg x && hasBareNameTrg x
      AnnAssign x _ _ -> hasSubscriptTrg x && hasBareNameTrg x
      _ -> False

doesntHaveMixedAssignment :: Program -> Bool
doesntHaveMixedAssignment = not . hasMixedAssignment

ensureDoesntHaveMixedAssignment :: MonadError Error m => Program -> m ()
ensureDoesntHaveMixedAssignment = makeEnsureProgram doesntHaveMixedAssignment "there must not be mixed assignments"

-- | `hasNonTrivialSubscriptedAssignmentInForLoops` checks that there are assignments with non-trivial subscriptions in for-loops.
-- A trivial subscription is a sequence of subscriptions to a variable with constant indices and at most one trivial loop-counter indices for each loops.
-- A constant index is an expr which has a constant value in the loop.
-- A trivial loop-counter index is the loop counter from "range(n)", "range(n, m)" or "enumerate(a)" with optional post-addition with a positive int literal.
--
-- For example, the followings have such assignments.
--
-- > x = 0
-- > for i in range(10):
-- >     x += 1
-- >     a[x] += 1
--
-- > for i in range(10):
-- >     j = i
-- >     a[j] += 1
--
-- > for i in range(10):
-- >     a[2 * i] += 1
--
-- > for i in range(10):
-- >     a[1 + i] += 1
--
-- > for i in range(10):
-- >     a[i - 1] += 1
--
-- > c = 1
-- > for i in range(10):
-- >     a[i + c] += 1
--
-- > for i in range(10):
-- >     a[i][i] += 1
--
-- > for i in [1, 2, 3]:
-- >     a[i] += 1
--
-- > b = range(10)
-- > for i in b:
-- >     a[i] += 1
--
-- > for i in range(0, 10, 2):
-- >     a[i] += 1
--
-- > for i, b_i in enumerate(b):
-- >     a[b_i] += i
--
-- For example, the followings don't have such assignments.
--
-- > c = 0
-- > for i in range(10):
-- >     a[c] += 1
--
-- > for i in range(10):
-- >     a[i] += 1
--
-- > for i in range(10):
-- >     a[i + 1] += 1
--
-- > for i in range(10):
-- >     for j in range(10):
-- >         a[i + 1][j] += 1
--
-- > for i in range(1, 10):
-- >     a[i] += 1
--
-- > for i, b_i in enumerate(b):
-- >     a[i] += b_i
hasNonTrivialSubscriptedAssignmentInForLoops :: Program -> Bool
hasNonTrivialSubscriptedAssignmentInForLoops prog = any check (listStatements prog)
  where
    check = \case
      AugAssign x _ _ -> go x
      AnnAssign x _ _ -> go x
      _ -> False
    go = \case
      SubscriptTrg _ _ -> False -- TODO
      NameTrg _ -> False
      TupleTrg xs -> any go xs

doesntHaveNonTrivialSubscriptedAssignmentInForLoops :: Program -> Bool
doesntHaveNonTrivialSubscriptedAssignmentInForLoops = not . hasMixedAssignment

ensureDoesntHaveNonTrivialSubscriptedAssignmentInForLoops :: MonadError Error m => Program -> m ()
ensureDoesntHaveNonTrivialSubscriptedAssignmentInForLoops = makeEnsureProgram doesntHaveNonTrivialSubscriptedAssignmentInForLoops "there must not be assignments with non-trivial subscriptions in for-loops"

-- | `hasAssginmentToBuiltin` checks that there are assignments to builtin functions.
-- For example, the followings have such assignments.
--
-- > map = 3
--
-- > return [range for range in range(10)]
hasAssignmentToBuiltin :: Program -> Bool
hasAssignmentToBuiltin _ = False -- TODO

doesntHaveAssignmentToBuiltin :: Program -> Bool
doesntHaveAssignmentToBuiltin = not . hasAssignmentToBuiltin

ensureDoesntHaveAssignmentToBuiltin :: MonadError Error m => Program -> m ()
ensureDoesntHaveAssignmentToBuiltin = makeEnsureProgram doesntHaveAssignmentToBuiltin "there must not be assignments to builtin functions"

-- | `hasNonResolvedBuiltin` checks that there are not resolved builtin functions.
-- This always doesn't hold after `Jikka.RestrictedPython.Language.Convert.ResolveBuiltin`.
hasNonResolvedBuiltin :: Program -> Bool
hasNonResolvedBuiltin = any check . listExprs
  where
    check = any check' . listSubExprs
    check' = \case
      Name x | x `S.member` builtinNames -> True
      _ -> False

doesntHaveNonResolvedBuiltin :: Program -> Bool
doesntHaveNonResolvedBuiltin = not . hasAssignmentToBuiltin

ensureDoesntHaveNonResolvedBuiltin :: MonadError Error m => Program -> m ()
ensureDoesntHaveNonResolvedBuiltin = makeEnsureProgram doesntHaveNonResolvedBuiltin "there must not be assignments to builtin functions"
