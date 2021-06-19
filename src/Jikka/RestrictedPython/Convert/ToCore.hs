{-# LANGUAGE FlexibleContexts #-}

module Jikka.RestrictedPython.Convert.ToCore
  ( run,
  )
where

import Jikka.Common.Error
import qualified Jikka.Core.Language.Expr as Y
import qualified Jikka.RestrictedPython.Language.Expr as X
import qualified Jikka.RestrictedPython.Language.Lint as X

-- | `run` converts programs of our restricted Python-like language to programs of our core language.
-- This assumes the follwing conditions:
--
-- * `X.doesntHaveSubscriptionInLoopCounters`
-- * `X.doesntHaveNameLeakOfLoopCounters`
-- * `X.doesntHaveAssignmentToLoopCounters`
-- * `X.doesntHaveAssignmentToLoopIterators`
-- * `X.doesntHaveReturnInLoops`
-- * `X.doesntHaveNonTrivialSubscriptedAssignmentInForLoops`
run :: MonadError Error m => X.Program -> m Y.Program
run prog = do
  X.ensureDoesntHaveSubscriptionInLoopCounters prog
  X.ensureDoesntHaveNameLeakOfLoopCounters prog
  X.ensureDoesntHaveAssignmentToLoopCounters prog
  X.ensureDoesntHaveAssignmentToLoopIterators prog
  X.ensureDoesntHaveReturnInLoops prog
  X.ensureDoesntHaveNonTrivialSubscriptedAssignmentInForLoops prog
  undefined
