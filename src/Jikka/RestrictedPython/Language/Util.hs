{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Language.Util where

import Control.Monad.Identity
import Control.Monad.Writer.Strict
import Data.List (delete, nub)
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.IO
import Jikka.RestrictedPython.Language.Expr

genType :: MonadAlpha m => m Type
genType = do
  i <- nextCounter
  return $ VarTy (TypeName ('$' : show i))

freeTyVars :: Type -> [TypeName]
freeTyVars = nub . go
  where
    go = \case
      VarTy x -> [x]
      IntTy -> []
      BoolTy -> []
      ListTy t -> go t
      TupleTy ts -> concat $ mapM go ts
      CallableTy ts ret -> concat $ mapM go (ret : ts)

freeVars :: Expr -> [VarName]
freeVars = nub . go
  where
    go = \case
      BoolOp e1 _ e2 -> go e1 ++ go e2
      BinOp e1 _ e2 -> go e1 ++ go e2
      UnaryOp _ e -> go e
      Lambda args e -> foldl (\vars (x, _) -> delete x vars) (go e) args
      IfExp e1 e2 e3 -> go e1 ++ go e2 ++ go e3
      ListComp e (Comprehension x iter pred) -> go iter ++ foldl (\vars x -> delete x vars) (go e ++ concatMap go pred) (targetVars x)
      Compare e1 _ e2 -> go e1 ++ go e2
      Call f args -> concatMap go (f : args)
      Constant _ -> []
      Subscript e1 e2 -> go e1 ++ go e2
      Name x -> [x]
      List _ es -> concatMap go es
      Tuple es -> concatMap go es
      SubscriptSlice e from to step -> go e ++ concatMap go from ++ concatMap go to ++ concatMap go step

freeVarsTarget :: Target -> [VarName]
freeVarsTarget = nub . go
  where
    go = \case
      SubscriptTrg _ e -> freeVars e
      NameTrg _ -> []
      TupleTrg xs -> concatMap go xs

targetVars :: Target -> [VarName]
targetVars = nub . go
  where
    go = \case
      SubscriptTrg x _ -> go x
      NameTrg x -> [x]
      TupleTrg xs -> concatMap go xs

mapStatementStatementM :: Monad m => (Statement -> m [Statement]) -> Statement -> m [Statement]
mapStatementStatementM f = \case
  Return e -> f $ Return e
  AugAssign x op e -> f $ AugAssign x op e
  AnnAssign x t e -> f $ AnnAssign x t e
  For x iter body -> do
    body <- concat <$> mapM (mapStatementStatementM f) body
    f $ For x iter body
  If e body1 body2 -> do
    body1 <- concat <$> mapM (mapStatementStatementM f) body1
    body2 <- concat <$> mapM (mapStatementStatementM f) body2
    f $ If e body1 body2
  Assert e -> f $ Assert e

mapStatementToplevelStatementM :: Monad m => (Statement -> m [Statement]) -> ToplevelStatement -> m ToplevelStatement
mapStatementToplevelStatementM go = \case
  ToplevelAnnAssign x t e -> return $ ToplevelAnnAssign x t e
  ToplevelFunctionDef f args ret body -> do
    body <- concat <$> mapM (mapStatementStatementM go) body
    return $ ToplevelFunctionDef f args ret body
  ToplevelAssert e -> return $ ToplevelAssert e

mapStatementM :: Monad m => (Statement -> m [Statement]) -> Program -> m Program
mapStatementM f = mapM (mapStatementToplevelStatementM f)

mapStatement :: (Statement -> [Statement]) -> Program -> Program
mapStatement f = runIdentity . mapStatementM (return . f)

mapLargeStatementM :: Monad m => (Expr -> [Statement] -> [Statement] -> m [Statement]) -> (Target -> Expr -> [Statement] -> m [Statement]) -> Program -> m Program
mapLargeStatementM fIf fFor = mapStatementM go
  where
    go = \case
      Return e -> return [Return e]
      AugAssign x op e -> return [AugAssign x op e]
      AnnAssign x t e -> return [AnnAssign x t e]
      For x iter body -> fFor x iter body
      If e body1 body2 -> fIf e body1 body2
      Assert e -> return [Assert e]

mapLargeStatement :: (Expr -> [Statement] -> [Statement] -> [Statement]) -> (Target -> Expr -> [Statement] -> [Statement]) -> Program -> Program
mapLargeStatement fIf fFor = runIdentity . mapLargeStatementM fIf' fFor'
  where
    fIf' e body1 body2 = return $ fIf e body1 body2
    fFor' x iter body = return $ fFor x iter body

listStatements :: Program -> [Statement]
listStatements = reverse . getDual . execWriter . mapStatementM go
  where
    go stmt = do
      tell $ Dual [stmt]
      return [stmt]

hasNoSubscriptTrg :: Target -> Bool
hasNoSubscriptTrg = \case
  SubscriptTrg _ _ -> False
  NameTrg _ -> True
  TupleTrg xs -> all hasNoSubscriptTrg xs

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

readValueIO :: (MonadIO m, MonadError Error m) => Type -> m Expr
readValueIO = \case
  VarTy _ -> throwRuntimeError "cannot read values of type variables"
  IntTy -> do
    n <- read <$> liftIO getWord
    return $ Constant (ConstInt n)
  BoolTy -> do
    p <- read <$> liftIO getWord
    return $ Constant (ConstBool p)
  ListTy t -> do
    n <- read <$> liftIO getWord
    xs <- replicateM n (readValueIO t)
    return $ List t xs
  TupleTy ts -> Tuple <$> mapM readValueIO ts
  CallableTy _ _ -> throwRuntimeError "cannot read functions"
