{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Convert.SplitLoops
  ( run,
    run',
    runForLoop,
  )
where

import Data.List (delete, intersect, nub, partition)
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.RestrictedPython.Convert.Alpha as Alpha (run)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util

newtype ReadList = ReadList [VarName]
  deriving (Eq, Ord, Show, Read)

newtype WriteList = WriteList [VarName]
  deriving (Eq, Ord, Show, Read)

hasEdge :: WriteList -> ReadList -> Bool
hasEdge (WriteList w) (ReadList r) = not (null (w `intersect` r))

analyzeExpr :: Expr -> ReadList
analyzeExpr = ReadList . freeVars

analyzeTargetRead :: Target -> ReadList
analyzeTargetRead = ReadList . freeVarsTarget

analyzeTargetWrite :: Target -> WriteList
analyzeTargetWrite = WriteList . targetVars

analyzeStatement :: Statement -> (ReadList, WriteList)
analyzeStatement = \case
  Return e -> (analyzeExpr e, WriteList [])
  AugAssign x _ e ->
    let w = analyzeTargetWrite x
        (ReadList r) = analyzeTargetRead x
        (ReadList r') = analyzeExpr e
     in (ReadList (r ++ r'), w)
  AnnAssign x _ e ->
    let w = analyzeTargetWrite x
        (ReadList r) = analyzeTargetRead x
        (ReadList r') = analyzeExpr e
     in (ReadList (r ++ r'), w)
  For x iter body ->
    let xs = targetVars x
        ReadList r = analyzeExpr iter
        (ReadList r', WriteList w) = analyzeStatements body
     in (ReadList (r ++ foldl (flip delete) r' xs), WriteList (foldl (flip delete) w xs))
  If e body1 body2 ->
    let ReadList r = analyzeExpr e
        (ReadList r1, WriteList w1) = analyzeStatements body1
        (ReadList r2, WriteList w2) = analyzeStatements body2
     in (ReadList (r ++ r1 ++ r2), WriteList (w1 ++ w2))
  Assert e -> (analyzeExpr e, WriteList [])

analyzeStatements :: [Statement] -> (ReadList, WriteList)
analyzeStatements = go [] []
  where
    go r w [] = (ReadList (nub r), WriteList (nub w))
    go r w (stmt : stmts) =
      let (ReadList r', WriteList w') = analyzeStatement stmt
       in go (r' ++ r) (w' ++ w) stmts

-- | `runForLoop` splits a for-loop to many for-loops as possible.
-- This assumes that `hasNoSubscriptionInLoopCounters`, `hasNoAssignToLoopCounters`, and `hasNoAssignToLoopIterators` hold.
--
-- This function analyzes read-variables and write-variables in statements, and split statements into connected components.
runForLoop :: Target -> Expr -> [Statement] -> [Statement]
runForLoop x iter body =
  let connected (_, (r, w)) (_, (r', w')) = hasEdge w r' || hasEdge w' r
      go result [] = reverse result
      go result (stmt : stmts) =
        let (same, diff) = partition (connected stmt) stmts
         in go (For x iter (map fst (stmt : same)) : result) diff
      body' = map (\stmt -> (stmt, analyzeStatement stmt)) body
   in go [] body'

-- | `run` splits for-loops into many small for-loops as possible.
-- This assumes that `hasNoSubscriptionInLoopCounters`, `hasNoAssignToLoopCounters`, and `hasNoAssignToLoopIterators` hold.
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

-- | `run` does `run'` and alpha conversion.
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run = Alpha.run . run'
