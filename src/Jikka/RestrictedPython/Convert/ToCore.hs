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

runConstant :: X.Constant -> Y.Expr
runConstant = \case
  X.ConstNone -> undefined -- TODO
  X.ConstInt n -> Y.Lit (Y.LitInt n)
  X.ConstBool p -> Y.Lit (Y.LitBool p)
  X.ConstBuiltin builtin -> runBuiltin builtin

runBuiltin :: X.Builtin -> Y.Expr
runBuiltin builtin =
  let f = Y.Lit . Y.LitBuiltin
   in case builtin of
        X.BuiltinAbs -> f Y.Abs
        X.BuiltinPow -> f Y.Pow
        X.BuiltinModPow -> undefined -- TODO
        X.BuiltinDivMod -> undefined -- TODO
        X.BuiltinCeilDiv -> f Y.CeilDiv
        X.BuiltinCeilMod -> f Y.CeilMod
        X.BuiltinFloorDiv -> f Y.FloorDiv
        X.BuiltinFloorMod -> f Y.FloorMod
        X.BuiltinGcd -> f Y.Gcd
        X.BuiltinLcm -> f Y.Lcm
        X.BuiltinInt _ -> undefined -- TODO
        X.BuiltinBool _ -> undefined -- TODO
        X.BuiltinList _ -> undefined -- TODO
        X.BuiltinTuple _ -> undefined -- TODO
        X.BuiltinLen t -> f $ Y.Len (runType t)
        X.BuiltinMap _ _ -> undefined -- TODO
        X.BuiltinSorted t -> f $ Y.Sorted (runType t)
        X.BuiltinReversed t -> f $ Y.Reversed (runType t)
        X.BuiltinEnumerate _ -> undefined -- TODO
        X.BuiltinFilter _ -> undefined -- TODO
        X.BuiltinZip _ -> undefined -- TODO
        X.BuiltinAll -> undefined -- TODO
        X.BuiltinAny -> undefined -- TODO
        X.BuiltinSum -> f Y.Sum
        X.BuiltinProduct -> f Y.Product
        X.BuiltinRange1 -> f Y.Range1
        X.BuiltinRange2 -> f Y.Range2
        X.BuiltinRange3 -> f Y.Range1
        X.BuiltinMax1 _ -> undefined -- TODO
        X.BuiltinMax _ _ -> undefined -- TODO
        X.BuiltinMin1 _ -> undefined -- TODO
        X.BuiltinMin _ _ -> undefined -- TODO
        X.BuiltinArgMax _ -> undefined -- TODO
        X.BuiltinArgMin _ -> undefined -- TODO
        X.BuiltinFact -> f Y.Fact
        X.BuiltinChoose -> f Y.Choose
        X.BuiltinPermute -> f Y.Permute
        X.BuiltinMultiChoose -> f Y.MultiChoose
        X.BuiltinModInv -> f Y.ModInv

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
  X.Max -> Y.Max2
  X.Min -> Y.Min2

runCmpOp :: X.CmpOp' -> Y.Builtin
runCmpOp (X.CmpOp' op _) = case op of
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
  X.Constant const -> return $ runConstant const
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
