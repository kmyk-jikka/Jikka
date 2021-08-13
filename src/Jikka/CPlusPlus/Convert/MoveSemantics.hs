{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Jikka.CPlusPlus.Convert.MoveSemantics
-- Description : removes unnecessary copying. / 無用なコピーを削除します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.CPlusPlus.Convert.MoveSemantics
  ( run,
  )
where

import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Jikka.CPlusPlus.Language.Expr
import Jikka.CPlusPlus.Language.Util
import Jikka.CPlusPlus.Language.VariableAnalysis
import Jikka.Common.Error

runExpr :: MonadState (M.Map VarName VarName) m => Expr -> m Expr
runExpr = \case
  Var x -> do
    y <- gets (M.lookup x)
    return $ Var (fromMaybe x y)
  Lit lit -> return $ Lit lit
  UnOp op e -> UnOp op <$> runExpr e
  BinOp op e1 e2 -> BinOp op <$> runExpr e1 <*> runExpr e2
  Cond e1 e2 e3 -> Cond <$> runExpr e1 <*> runExpr e2 <*> runExpr e3
  Lam args ret body -> Lam args ret <$> runStatements body []
  Call f args -> Call f <$> mapM runExpr args
  CallExpr f args -> CallExpr <$> runExpr f <*> mapM runExpr args

runLeftExpr :: MonadState (M.Map VarName VarName) m => LeftExpr -> m LeftExpr
runLeftExpr = \case
  LeftVar x -> do
    y <- gets (M.lookup x)
    return $ LeftVar (fromMaybe x y)
  LeftAt e1 e2 -> LeftAt <$> runLeftExpr e1 <*> runExpr e2
  LeftGet n e -> LeftGet n <$> runLeftExpr e

runAssignExpr :: MonadState (M.Map VarName VarName) m => AssignExpr -> m AssignExpr
runAssignExpr = \case
  AssignExpr op e1 e2 -> AssignExpr op <$> runLeftExpr e1 <*> runExpr e2
  AssignIncr e -> AssignIncr <$> runLeftExpr e
  AssignDecr e -> AssignDecr <$> runLeftExpr e

data CopyType
  = SimpleCopy
  | UpdatedCopy
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | @isMovableTo x y env cont@ checkes whether @x@ is movable to @y@ instead of copying, under a context @env@ and @cont@.
-- @x@ is already replaced by the mapping of @env@, but @y@ and @cont@ are not yet.
isMovableTo :: VarName -> VarName -> CopyType -> M.Map VarName VarName -> [[Statement]] -> Bool
isMovableTo x y0 typ env cont
  | x `S.notMember` readList' (analyzeStatements' (concat cont)) = True -- @x@ is movable if @x@ is not used after the current position
  | otherwise =
    let go = \case
          [] -> False
          (Assign (AssignExpr SimpleAssign (LeftVar x') (Var _)) : cont')
            | x' == x -> -- re-assignment to @x@
              let ReadWriteList _ ws' = analyzeStatements' cont'
                  ReadWriteList rs ws = analyzeStatements' (concat (tail cont))
               in y `S.notMember` S.unions [ws', rs, ws]
          (stmt : cont) ->
            let ReadWriteList rs ws = analyzeStatement' stmt
                rws = if typ == SimpleCopy then ws else S.union rs ws -- Reading @x@ is okay when it's a copy without updating.
             in x `S.notMember` rws && go cont -- @x@ is used
     in go (head cont) -- @x@ is movable if @x@ is unused until a next re-assignment
  where
    y :: VarName
    y = fromMaybe y0 (M.lookup y0 env)
    applyEnv :: ReadWriteList -> ReadWriteList
    applyEnv (ReadWriteList rs ws) =
      let f = S.map (\x -> fromMaybe x (M.lookup x env))
       in ReadWriteList (f rs) (f ws)
    analyzeStatements' = applyEnv . analyzeStatements
    analyzeStatement' = applyEnv . analyzeStatement

runStatement :: MonadState (M.Map VarName VarName) m => Statement -> [[Statement]] -> m [Statement]
runStatement stmt cont = case stmt of
  ExprStatement e -> do
    e <- runExpr e
    return [ExprStatement e]
  Block stmts -> do
    runStatements stmts cont
  If e body1 body2 -> do
    e <- runExpr e
    body1 <- runStatements body1 cont
    body2 <- traverse (`runStatements` cont) body2
    return [If e body1 body2]
  For t x init pred incr body -> do
    init <- runExpr init
    pred <- runExpr pred
    incr <- runAssignExpr incr
    body <- runStatements body cont
    return [For t x init pred incr body]
  ForEach t x e body -> do
    e <- runExpr e
    body <- runStatements body cont
    return [ForEach t x e body]
  While e body -> do
    e <- runExpr e
    body <- runStatements body cont
    return [While e body]
  Declare t y init -> do
    init <- case init of
      DeclareDefault -> return DeclareDefault
      DeclareCopy e -> DeclareCopy <$> runExpr e
      DeclareInitialize es -> DeclareInitialize <$> mapM runExpr es
    env <- get
    case init of
      DeclareCopy (Var x) | (x `isMovableTo` y) SimpleCopy env cont -> do
        modify' (M.insert y x)
        return []
      DeclareCopy (Call (SetAt _) [Var x, i, xi])
        | (x `isMovableTo` y) UpdatedCopy env cont -> do
          modify' (M.insert y x)
          return [Assign (AssignExpr SimpleAssign (LeftAt (LeftVar x) i) xi)]
      DeclareCopy (Call ConvexHullTrickCtor []) -> return [Declare t y DeclareDefault]
      DeclareCopy (Call ConvexHullTrickCopyAddLine [Var x, a, b])
        | (x `isMovableTo` y) UpdatedCopy env cont -> do
          modify' (M.insert y x)
          return [callMethod' (Var x) "add_line" [a, b]]
      DeclareCopy (Call (SegmentTreeCopySetPoint _) [Var x, i, a])
        | (x `isMovableTo` y) UpdatedCopy env cont -> do
          modify' (M.insert y x)
          return [callMethod' (Var x) "set" [i, a]]
      _ -> do
        return [Declare t y init]
  DeclareDestructure xs e -> do
    e <- runExpr e
    return [DeclareDestructure xs e]
  Assign e -> do
    e <- runAssignExpr e
    env <- get
    case e of
      AssignExpr SimpleAssign (LeftVar y) (Var x) | x == y -> return []
      AssignExpr SimpleAssign (LeftVar y) (Call (SetAt _) [Var x, i, xi])
        | x == y -> return [Assign (AssignExpr SimpleAssign (LeftAt (LeftVar x) i) xi)]
        | (x `isMovableTo` y) UpdatedCopy env cont -> do
          modify' (M.insert y x)
          return [Assign (AssignExpr SimpleAssign (LeftAt (LeftVar x) i) xi)]
        | otherwise -> return [Assign e]
      AssignExpr SimpleAssign (LeftVar y) (Call ConvexHullTrickCopyAddLine [Var x, a, b])
        | x == y -> return [callMethod' (Var x) "add_line" [a, b]]
        | (x `isMovableTo` y) UpdatedCopy env cont -> do
          modify' (M.insert y x)
          return [callMethod' (Var x) "add_line" [a, b]]
        | otherwise -> return [Assign e]
      AssignExpr SimpleAssign (LeftVar y) (Call (SegmentTreeCopySetPoint _) [Var x, i, a])
        | x == y -> return [callMethod' (Var x) "set" [i, a]]
        | (x `isMovableTo` y) UpdatedCopy env cont -> do
          modify' (M.insert y x)
          return [callMethod' (Var x) "set" [i, a]]
        | otherwise -> return [Assign e]
      _ -> return [Assign e]
  Assert e -> do
    e <- runExpr e
    return [Assert e]
  Return e -> do
    e <- runExpr e
    return [Return e]

runStatements :: MonadState (M.Map VarName VarName) m => [Statement] -> [[Statement]] -> m [Statement]
runStatements stmts cont = case stmts of
  [] -> return []
  stmt : stmts -> do
    stmt <- runStatement stmt (stmts : cont)
    stmts <- runStatements stmts cont
    return (stmt ++ stmts)

runToplevelStatement :: MonadState (M.Map VarName VarName) m => ToplevelStatement -> m ToplevelStatement
runToplevelStatement = \case
  VarDef t x e -> VarDef t x <$> runExpr e
  FunDef ret f args body -> FunDef ret f args <$> runStatements body []
  StaticAssert e msg -> StaticAssert <$> runExpr e <*> pure msg

runProgram :: Monad m => Program -> m Program
runProgram (Program decls) = (`evalStateT` M.empty) $ do
  Program <$> mapM runToplevelStatement decls

-- | `run` replaces superfluous copying.
--
-- == Examples
--
-- Before:
--
-- > vector<int> solve(vector<int> a) {
-- >     vector<int> b = a;
-- >     b[0] = 1;
-- >     return b;
-- > }
--
-- After:
--
-- > vector<int> solve(vector<int> a) {
-- >     a[0] = 1;
-- >     return a;
-- > }
--
-- Before:
--
-- > int solve(int a, int b, int x) {
-- >     jikka::convex_hull_trick cht = jikka::convex_hull_trick();
-- >     cht = jikka::convex_hull_trick::persistent_add_line(cht, a, b);
-- >     return cht.get_min(x);
-- > }
--
-- After:
--
-- > int solve(int a, int b, int x) {
-- >     jikka::convex_hull_trick cht;
-- >     cht = cht.add_line(a, b);
-- >     return cht.get_min(x);
-- > }
run :: MonadError Error m => Program -> m Program
run prog = wrapError' "Jikka.CPlusPlus.Convert.MoveSemantics" $ do
  runProgram prog
