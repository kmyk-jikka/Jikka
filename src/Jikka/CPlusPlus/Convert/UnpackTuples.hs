{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.CPlusPlus.Convert.UnpackTuples
-- Description : unpack tuples. / タプルを展開します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.CPlusPlus.Convert.UnpackTuples
  ( run,
  )
where

import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Set as S
import Jikka.CPlusPlus.Language.Expr
import Jikka.CPlusPlus.Language.Util
import Jikka.Common.Alpha
import Jikka.Common.Error

-- | `runExpr` replaces variables using the @mapping :: M.Map VarName [(Type, VarName)]@.
runExpr :: (MonadAlpha m, MonadError Error m, MonadState (M.Map VarName [(Type, VarName)]) m) => Expr -> m Expr
runExpr = \case
  Var x -> do
    ys <- gets (M.lookup x)
    return $ case ys of
      Nothing -> Var x
      Just ys ->
        let es = map (Var . snd) ys
         in if shouldBeArray (map fst ys)
              then
                let t = fst (head ys)
                 in Call (ArrayExt t) es
              else
                let ts = map fst ys
                 in Call (StdTuple ts) es
  Lit lit -> return $ Lit lit
  UnOp op e -> UnOp op <$> runExpr e
  BinOp op e1 e2 -> BinOp op <$> runExpr e1 <*> runExpr e2
  Cond e1 e2 e3 -> Cond <$> runExpr e1 <*> runExpr e2 <*> runExpr e3
  Lam args ret body -> Lam args ret <$> runStatements body []
  Call f args -> runCall f args
  CallExpr e args -> CallExpr <$> runExpr e <*> mapM runExpr args

-- | `runCall` does the same thing to `runExpr` and also reduces `std::get<i>(e)` and `e[i]`.
runCall :: (MonadAlpha m, MonadError Error m, MonadState (M.Map VarName [(Type, VarName)]) m) => Function -> [Expr] -> m Expr
runCall f args = do
  args <- mapM runExpr args
  case (f, args) of
    -- std::get<n>(x)
    (StdGet n, [Var x]) -> do
      ys <- gets (M.lookup x)
      case ys of
        Just ys -> do
          let es = map (Var . snd) ys
          when (n < 0 || toInteger (length ys) <= n) $ do
            throwInternalError "index out of range"
          return $ es !! fromInteger n
        Nothing -> return $ Call f args
    -- std::get<n>(std::tuple<T1, T2, ...>(e1, e2, ...))
    (StdGet n, [Call (StdTuple _) es]) -> do
      when (n < 0 || toInteger (length es) <= n) $ do
        throwInternalError "index out of range"
      return $ es !! fromInteger n
    -- x[i]
    (At, [Var x, e2]) -> do
      ys <- gets (M.lookup x)
      case ys of
        Just ys -> do
          let es = map (Var . snd) ys
          let n = case e2 of
                Lit (LitInt32 n) -> Just n
                Lit (LitInt64 n) -> Just n
                _ -> Nothing
          case n of
            Just n -> do
              when (n < 0 || toInteger (length ys) <= n) $ do
                throwInternalError "index out of range"
              return (es !! fromInteger n)
            Nothing -> return $ Call f args
        Nothing -> return $ Call f args
    -- (std::array<T, n>{e1, e2, ...})[i]
    (At, [Call (ArrayExt _) es, e2]) -> do
      let n = case e2 of
            Lit (LitInt32 n) -> Just n
            Lit (LitInt64 n) -> Just n
            _ -> Nothing
      case n of
        Just n -> do
          when (n < 0 || toInteger (length es) <= n) $ do
            throwInternalError "index out of range"
          return (es !! fromInteger n)
        Nothing -> return $ Call f args
    _ -> return $ Call f args

runLeftExpr :: (MonadAlpha m, MonadError Error m, MonadState (M.Map VarName [(Type, VarName)]) m) => LeftExpr -> m LeftExpr
runLeftExpr = \case
  LeftVar x -> return $ LeftVar x -- do nothing
  LeftAt e1 e2 -> LeftAt <$> runLeftExpr e1 <*> runExpr e2
  LeftGet n e -> LeftGet n <$> runLeftExpr e

runAssignExpr :: (MonadAlpha m, MonadError Error m, MonadState (M.Map VarName [(Type, VarName)]) m) => AssignExpr -> m AssignExpr
runAssignExpr = \case
  AssignExpr op e1 e2 -> AssignExpr op <$> runLeftExpr e1 <*> runExpr e2
  AssignIncr e -> AssignIncr <$> runLeftExpr e
  AssignDecr e -> AssignDecr <$> runLeftExpr e

-- | `runStatement` expands assignments to variables of @std::tuple<T1, T2, ...>@ and @std::array<T, n>@.
runStatement :: (MonadAlpha m, MonadError Error m, MonadState (M.Map VarName [(Type, VarName)]) m) => Statement -> [[Statement]] -> m [Statement]
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
  Declare t x init -> do
    init <- case init of
      DeclareDefault -> return DeclareDefault
      DeclareCopy e -> DeclareCopy <$> runExpr e
      DeclareInitialize es -> DeclareInitialize <$> mapM runExpr es
    case init of
      -- std::tuple<T1, T2, ...> x = std::tuple<...>(e1, e2, ...);
      DeclareCopy (Call (StdTuple ts) es) -> do
        ys <- replicateM (length es) (renameVarName LocalNameKind (unVarName x))
        modify' (M.insert x (zip ts ys))
        return $ zipWith3 (\t y e -> Declare t y (DeclareCopy e)) ts ys es
      -- std::array<T, n> x = std::array<T, n>{e1, e2, ...};
      DeclareCopy (Call (ArrayExt t) es) -> do
        let ts = replicate (length es) t
        ys <- replicateM (length es) (renameVarName LocalNameKind (unVarName x))
        modify' (M.insert x (zip ts ys))
        return $ zipWith3 (\t y e -> Declare t y (DeclareCopy e)) ts ys es
      _ -> do
        return [Declare t x init]
  DeclareDestructure xs e -> do
    e <- runExpr e
    return [DeclareDestructure xs e]
  Assign e -> do
    e <- runAssignExpr e
    case e of
      -- x = e;
      AssignExpr SimpleAssign (LeftVar x) e -> do
        ys <- gets (M.lookup x)
        case ys of
          Just ys -> do
            let ts = map fst ys
            let n = toInteger (length ts)
            let es = case e of
                  Call (StdTuple _) es -> es
                  Call (ArrayExt _) es -> es
                  _ ->
                    if shouldBeArray ts
                      then map (\i -> Call At [e, litInt32 i]) [0 .. n - 1]
                      else map (\i -> Call (StdGet i) [e]) [0 .. n - 1]
            tmpys <- replicateM (length ts) (newFreshName LocalNameKind)
            return $ zipWith3 (\t y e -> Declare t y (DeclareCopy e)) ts tmpys es ++ zipWith (\y e -> Assign (AssignExpr SimpleAssign (LeftVar y) (Var e))) (map snd ys) tmpys
          Nothing -> return [Assign (AssignExpr SimpleAssign (LeftVar x) e)]
      _ -> do
        forM_ (S.toList (freeVarsAssignExpr e)) $ \x -> do
          ys <- gets (M.lookup x)
          case ys of
            Just _ -> throwInternalError $ "wrong assignment to a tuple: " ++ unVarName x
            Nothing -> return ()
        return [Assign e]
  Assert e -> do
    e <- runExpr e
    return [Assert e]
  Return e -> do
    e <- runExpr e
    return [Return e]

runStatements :: (MonadAlpha m, MonadError Error m, MonadState (M.Map VarName [(Type, VarName)]) m) => [Statement] -> [[Statement]] -> m [Statement]
runStatements stmts cont = case stmts of
  [] -> return []
  stmt : stmts -> do
    stmt <- runStatement stmt (stmts : cont)
    stmts <- runStatements stmts cont
    return (stmt ++ stmts)

runToplevelStatement :: (MonadAlpha m, MonadError Error m, MonadState (M.Map VarName [(Type, VarName)]) m) => ToplevelStatement -> m ToplevelStatement
runToplevelStatement = \case
  VarDef t x e -> VarDef t x <$> runExpr e
  FunDef ret f args body -> FunDef ret f args <$> runStatements body []

runProgram :: (MonadAlpha m, MonadError Error m) => Program -> m Program
runProgram (Program decls) = (`evalStateT` M.empty) $ do
  Program <$> mapM runToplevelStatement decls

-- | `run` unpack tuples.
--
-- == Examples
--
-- Before:
--
-- > tuple<int, int> c = make_tuple(a, b);
-- > func(get<0>(c), get<1>(c));
--
-- After:
--
-- > int c0 = a;
-- > int c1 = b;
-- > func(c0, c1);
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.CPlusPlus.Convert.UnpackTuples" $ do
  runProgram prog
