{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Convert.ToCore
  ( run,
  )
where

import Control.Arrow ((***))
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Language.Expr as Y
import qualified Jikka.Core.Language.Util as Y
import qualified Jikka.RestrictedPython.Language.Expr as X
import qualified Jikka.RestrictedPython.Language.Lint as X

runVarName :: X.VarName -> Y.VarName
runVarName (X.VarName x) = Y.VarName x

runType :: X.Type -> Y.Type
runType = \case
  X.VarTy (X.TypeName x) -> Y.VarTy (Y.TypeName x)
  X.IntTy -> Y.IntTy
  X.BoolTy -> Y.BoolTy
  X.ListTy t -> Y.ListTy (runType t)
  X.TupleTy ts -> Y.TupleTy (map runType ts)
  X.CallableTy args ret -> Y.FunTy (map runType args) (runType ret)

runConstant :: X.Constant -> Y.Literal
runConstant = \case
  X.ConstNone -> undefined -- TODO
  X.ConstInt n -> Y.LitInt n
  X.ConstBool p -> Y.LitBool p

runBoolOp :: X.BoolOp -> Y.Builtin
runBoolOp = \case
  X.And -> Y.And
  X.Or -> Y.Or
  X.Implies -> Y.Implies

runUnaryOp :: X.UnaryOp -> Y.Builtin
runUnaryOp = \case
  X.Invert -> Y.BitNot
  X.Not -> Y.Not
  X.UAdd -> undefined -- TODO
  X.USub -> Y.Negate

runOperator :: X.Operator -> Y.Builtin
runOperator = \case
  X.Add -> Y.Plus
  X.Sub -> Y.Minus
  X.Mult -> Y.Mult
  X.MatMult -> undefined -- TODO
  X.Div -> undefined -- TODO
  X.FloorDiv -> Y.FloorDiv
  X.FloorMod -> Y.FloorMod
  X.CeilDiv -> Y.CeilDiv
  X.CeilMod -> Y.CeilMod
  X.Pow -> Y.Pow
  X.BitLShift -> Y.BitLeftShift
  X.BitRShift -> Y.BitRightShift
  X.BitOr -> Y.BitOr
  X.BitXor -> Y.BitXor
  X.BitAnd -> Y.BitAnd
  X.Max -> Y.Max
  X.Min -> Y.Min

runCmpOp :: X.CmpOp -> Y.Builtin
runCmpOp = \case
  X.Lt -> Y.LessThan
  X.LtE -> Y.LessEqual
  X.Gt -> Y.GreaterThan
  X.GtE -> Y.GreaterEqual
  X.Eq' -> Y.Equal undefined -- TODO
  X.NotEq -> Y.NotEqual undefined -- TODO
  X.Is -> undefined -- TODO
  X.IsNot -> undefined -- TODO
  X.In -> undefined -- TODO
  X.NotIn -> undefined -- TODO

makeList2 :: a -> a -> [a]
makeList2 x y = [x, y]

runExpr :: (MonadAlpha m, MonadError Error m) => X.Expr -> m Y.Expr
runExpr = \case
  X.BoolOp e1 op e2 -> Y.AppBuiltin (runBoolOp op) <$> (makeList2 <$> runExpr e1 <*> runExpr e2)
  X.BinOp e1 op e2 -> Y.AppBuiltin (runOperator op) <$> (makeList2 <$> runExpr e1 <*> runExpr e2)
  X.UnaryOp op e -> Y.AppBuiltin (runUnaryOp op) . (: []) <$> runExpr e
  X.Lambda _ _ -> undefined -- TODO
  X.IfExp e1 e2 e3 -> do
    e1 <- runExpr e1
    e2 <- runExpr e2
    e3 <- runExpr e3
    t <- Y.genType
    return $ Y.AppBuiltin (Y.If t) [e1, e2, e3]
  X.ListComp _ (X.Comprehension _ _ _) -> undefined -- TODO
  X.Compare e1 op e2 -> Y.AppBuiltin (runCmpOp op) <$> (makeList2 <$> runExpr e1 <*> runExpr e2)
  X.Call f args -> Y.App <$> runExpr f <*> mapM runExpr args
  X.Constant const -> return $ Y.Lit (runConstant const)
  X.Subscript e1 e2 -> Y.AppBuiltin <$> (Y.At <$> Y.genType) <*> (makeList2 <$> runExpr e1 <*> runExpr e2)
  X.Name x -> return $ Y.Var (runVarName x)
  X.List _ _ -> undefined -- TODO
  X.Tuple _ -> undefined -- TODO
  X.SubscriptSlice _ _ _ _ -> undefined -- TODO

runStatements :: (MonadAlpha m, MonadError Error m) => [X.Statement] -> m Y.Expr
runStatements [] = throwSemanticError "function may not return"
runStatements (stmt : stmts) = case stmt of
  X.Return e -> runExpr e
  X.AugAssign _ _ _ -> undefined -- TODO
  X.AnnAssign _ _ _ -> undefined -- TODO
  X.For _ _ _ -> undefined -- TODO
  X.If e body1 body2 -> do
    e <- runExpr e
    body1 <- runStatements (body1 ++ stmts)
    body2 <- runStatements (body2 ++ stmts)
    t <- Y.genType
    return $ Y.AppBuiltin (Y.If t) [e, body1, body2]
  X.Assert _ -> runStatements stmts

runToplevelStatements :: (MonadAlpha m, MonadError Error m) => [X.ToplevelStatement] -> m Y.ToplevelExpr
runToplevelStatements [] = return $ Y.ResultExpr (Y.Var (Y.VarName "solve"))
runToplevelStatements (stmt : stmts) = case stmt of
  X.ToplevelAnnAssign _ _ _ -> undefined -- TODO
  X.ToplevelFunctionDef f args ret body -> Y.ToplevelLet Y.Rec (runVarName f) (map (runVarName *** runType) args) (runType ret) <$> runStatements body <*> runToplevelStatements stmts
  X.ToplevelAssert _ -> runToplevelStatements stmts -- TOOD: use assertions as hints

-- | `run` converts programs of our restricted Python-like language to programs of our core language.
-- This assumes the follwing conditions:
--
-- * `X.doesntHaveSubscriptionInLoopCounters`
-- * `X.doesntHaveLeakOfLoopCounters`
-- * `X.doesntHaveAssignmentToLoopCounters`
-- * `X.doesntHaveAssignmentToLoopIterators`
-- * `X.doesntHaveReturnInLoops`
-- * `X.doesntHaveNonTrivialSubscriptedAssignmentInForLoops`
run :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
run prog = do
  X.ensureDoesntHaveSubscriptionInLoopCounters prog
  X.ensureDoesntHaveLeakOfLoopCounters prog
  X.ensureDoesntHaveAssignmentToLoopCounters prog
  X.ensureDoesntHaveAssignmentToLoopIterators prog
  X.ensureDoesntHaveReturnInLoops prog
  X.ensureDoesntHaveNonTrivialSubscriptedAssignmentInForLoops prog
  runToplevelStatements prog
