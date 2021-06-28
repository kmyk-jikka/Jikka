{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Language.VariableAnalysis where

import Data.List (delete, intersect, nub)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util

newtype ReadList = ReadList [VarName]
  deriving (Eq, Ord, Show, Read)

newtype WriteList = WriteList [VarName]
  deriving (Eq, Ord, Show, Read)

haveWriteReadIntersection :: WriteList -> ReadList -> Bool
haveWriteReadIntersection (WriteList w) (ReadList r) = not (null (w `intersect` r))

analyzeExpr :: Expr' -> ReadList
analyzeExpr = ReadList . freeVars

analyzeTargetRead :: Target' -> ReadList
analyzeTargetRead = ReadList . freeVarsTarget

analyzeTargetWrite :: Target' -> WriteList
analyzeTargetWrite = WriteList . targetVars

analyzeStatementGeneric :: Bool -> Statement -> (ReadList, WriteList)
analyzeStatementGeneric isMax = \case
  Return e -> (analyzeExpr e, WriteList [])
  AugAssign x _ e ->
    let w = analyzeTargetWrite x
        (ReadList r) = analyzeTargetRead x
        (ReadList r') = analyzeExpr e
     in (ReadList (nub $ r ++ r'), w)
  AnnAssign x _ e ->
    let w = analyzeTargetWrite x
        (ReadList r) = analyzeTargetRead x
        (ReadList r') = analyzeExpr e
     in (ReadList (nub $ r ++ r'), w)
  For x iter body ->
    let xs = targetVars x
        ReadList r = analyzeExpr iter
        (ReadList r', WriteList w) = analyzeStatementsGeneric isMax body
     in if isMax
          then (ReadList (nub $ r ++ foldl (flip delete) r' xs), WriteList (nub $ foldl (flip delete) w xs))
          else (ReadList r, WriteList [])
  If e body1 body2 ->
    let ReadList r = analyzeExpr e
        (ReadList r1, WriteList w1) = analyzeStatementsGeneric isMax body1
        (ReadList r2, WriteList w2) = analyzeStatementsGeneric isMax body2
     in if isMax
          then (ReadList (nub $ r ++ r1 ++ r2), WriteList (nub $ w1 ++ w2))
          else (ReadList (nub $ r ++ intersect r1 r2), WriteList (nub $ w1 `intersect` w2))
  Assert e -> (analyzeExpr e, WriteList [])

analyzeStatementsGeneric :: Bool -> [Statement] -> (ReadList, WriteList)
analyzeStatementsGeneric isMax = go [] []
  where
    go r w [] = (ReadList (nub r), WriteList (nub w))
    go r w (stmt : stmts) =
      let (ReadList r', WriteList w') = analyzeStatementGeneric isMax stmt
       in go (r' ++ r) (w' ++ w) stmts

-- | `analyzeStatementMax` returns lists of variables which are possibly read or written in given statements.
analyzeStatementMax :: Statement -> (ReadList, WriteList)
analyzeStatementMax = analyzeStatementGeneric True

analyzeStatementsMax :: [Statement] -> (ReadList, WriteList)
analyzeStatementsMax = analyzeStatementsGeneric True

-- | `analyzeStatementMin` returns lists of variables which are always read or written in given statements.
analyzeStatementMin :: Statement -> (ReadList, WriteList)
analyzeStatementMin = analyzeStatementGeneric False

analyzeStatementsMin :: [Statement] -> (ReadList, WriteList)
analyzeStatementsMin = analyzeStatementsGeneric False
