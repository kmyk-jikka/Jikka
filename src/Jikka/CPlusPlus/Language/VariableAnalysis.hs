{-# LANGUAGE LambdaCase #-}

module Jikka.CPlusPlus.Language.VariableAnalysis where

import qualified Data.Set as S
import Jikka.CPlusPlus.Language.Expr

data ReadWriteList = ReadWriteList
  { readList' :: S.Set VarName,
    writeList :: S.Set VarName
  }
  deriving (Eq, Ord, Show, Read)

instance Semigroup ReadWriteList where
  ReadWriteList rs ws <> ReadWriteList rs' ws' = ReadWriteList (rs <> rs') (ws <> ws')

instance Monoid ReadWriteList where
  mempty = ReadWriteList S.empty S.empty

readVariable :: VarName -> ReadWriteList
readVariable x = ReadWriteList (S.singleton x) S.empty

writeVariable :: VarName -> ReadWriteList
writeVariable x = ReadWriteList S.empty (S.singleton x)

analyzeExpr :: Expr -> ReadWriteList
analyzeExpr = \case
  Var x -> readVariable x
  Lit _ -> mempty
  UnOp _ e -> analyzeExpr e
  BinOp _ e1 e2 -> analyzeExpr e1 <> analyzeExpr e2
  Cond e1 e2 e3 -> analyzeExpr e1 <> analyzeExpr e2 <> analyzeExpr e3
  Lam args _ body ->
    let ReadWriteList rs ws = analyzeStatements body
        args' = S.fromList (map snd args)
     in ReadWriteList (rs `S.difference` args') (ws `S.difference` args')
  Call _ args -> mconcat (map analyzeExpr args)
  CallExpr f args -> mconcat (map analyzeExpr (f : args))

analyzeLeftExpr :: LeftExpr -> ReadWriteList
analyzeLeftExpr = \case
  LeftVar x -> writeVariable x
  LeftAt e1 e2 -> analyzeLeftExpr e1 <> analyzeExpr e2
  LeftGet _ e -> analyzeLeftExpr e

analyzeAssignExpr :: AssignExpr -> ReadWriteList
analyzeAssignExpr = \case
  AssignExpr _ e1 e2 -> analyzeLeftExpr e1 <> analyzeExpr e2
  AssignIncr e -> analyzeLeftExpr e
  AssignDecr e -> analyzeLeftExpr e

analyzeStatement :: Statement -> ReadWriteList
analyzeStatement = \case
  ExprStatement e -> analyzeExpr e
  Block body -> analyzeStatements body
  If e body1 body2 -> analyzeExpr e <> analyzeStatements body1 <> maybe mempty analyzeStatements body2
  For _ x init pred incr body -> writeVariable x <> analyzeExpr init <> analyzeExpr pred <> analyzeAssignExpr incr <> analyzeStatements body
  ForEach _ x e body -> writeVariable x <> analyzeExpr e <> analyzeStatements body
  While e body -> analyzeExpr e <> analyzeStatements body
  Declare _ x init ->
    writeVariable x <> case init of
      DeclareDefault -> mempty
      DeclareCopy e -> analyzeExpr e
      DeclareInitialize es -> mconcat (map analyzeExpr es)
  DeclareDestructure xs e -> mconcat (map writeVariable xs) <> analyzeExpr e
  Assign e -> analyzeAssignExpr e
  Assert e -> analyzeExpr e
  Return e -> analyzeExpr e

analyzeStatements :: [Statement] -> ReadWriteList
analyzeStatements = mconcat . map analyzeStatement
