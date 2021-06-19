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
