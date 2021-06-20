{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Jikka.RestrictedPython.Convert.ToCore
  ( run,
  )
where

import Control.Arrow ((***))
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Language.BuiltinPatterns as Y
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

runConstant :: MonadError Error m => X.Constant -> m Y.Expr
runConstant = \case
  X.ConstNone -> undefined -- TODO
  X.ConstInt n -> return $ Y.Lit (Y.LitInt n)
  X.ConstBool p -> return $ Y.Lit (Y.LitBool p)
  X.ConstBuiltin builtin -> runBuiltin builtin

runBuiltin :: MonadError Error m => X.Builtin -> m Y.Expr
runBuiltin builtin =
  let f = return . Y.Lit . Y.LitBuiltin
   in case builtin of
        X.BuiltinAbs -> f Y.Abs
        X.BuiltinPow -> f Y.Pow
        X.BuiltinModPow -> f Y.ModPow
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
        X.BuiltinAll -> f Y.All
        X.BuiltinAny -> f Y.Any
        X.BuiltinSum -> f Y.Sum
        X.BuiltinProduct -> f Y.Product
        X.BuiltinRange1 -> f Y.Range1
        X.BuiltinRange2 -> f Y.Range2
        X.BuiltinRange3 -> f Y.Range1
        X.BuiltinMax1 t -> f $ Y.Max1 (runType t)
        X.BuiltinMax t n ->
          if n < 2
            then throwTypeError $ "max expected 2 or more arguments, got " ++ show n
            else
              let t' = runType t
                  args = map (\i -> Y.VarName ('x' : show i)) [0 .. n -1]
               in return $ Y.Lam (map (,t') args) (foldr1 (Y.Max2' t') (map Y.Var args))
        X.BuiltinMin1 t -> f $ Y.Min1 (runType t)
        X.BuiltinMin t n ->
          if n < 2
            then throwTypeError $ "max min 2 or more arguments, got " ++ show n
            else
              let t' = runType t
                  args = map (\i -> Y.VarName ('x' : show i)) [0 .. n -1]
               in return $ Y.Lam (map (,t') args) (foldr1 (Y.Min2' t') (map Y.Var args))
        X.BuiltinArgMax t -> f $ Y.ArgMax (runType t)
        X.BuiltinArgMin t -> f $ Y.ArgMin (runType t)
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

runUnaryOp :: X.UnaryOp -> Y.Expr
runUnaryOp =
  let f = Y.Lit . Y.LitBuiltin
   in \case
        X.Invert -> f Y.BitNot
        X.Not -> f Y.Not
        X.UAdd -> Y.Lam [("x", Y.IntTy)] (Y.Var "x")
        X.USub -> f Y.Negate

runOperator :: MonadError Error m => X.Operator -> m Y.Builtin
runOperator = \case
  X.Add -> return Y.Plus
  X.Sub -> return Y.Minus
  X.Mult -> return Y.Mult
  X.MatMult -> throwSemanticError "matmul operator ('@') is not supported"
  X.Div -> throwSemanticError "floatdiv operator ('/') is not supported"
  X.FloorDiv -> return Y.FloorDiv
  X.FloorMod -> return Y.FloorMod
  X.CeilDiv -> return Y.CeilDiv
  X.CeilMod -> return Y.CeilMod
  X.Pow -> return Y.Pow
  X.BitLShift -> return Y.BitLeftShift
  X.BitRShift -> return Y.BitRightShift
  X.BitOr -> return Y.BitOr
  X.BitXor -> return Y.BitXor
  X.BitAnd -> return Y.BitAnd
  X.Max -> return $ Y.Max2 Y.IntTy
  X.Min -> return $ Y.Min2 Y.IntTy

runCmpOp :: X.CmpOp' -> Y.Expr
runCmpOp (X.CmpOp' op t) =
  let t' = runType t
      f = Y.Lit . Y.LitBuiltin
   in case op of
        X.Lt -> f $ Y.LessThan t'
        X.LtE -> f $ Y.LessEqual t'
        X.Gt -> f $ Y.GreaterThan t'
        X.GtE -> f $ Y.GreaterEqual t'
        X.Eq' -> f $ Y.Equal t'
        X.NotEq -> f $ Y.NotEqual t'
        X.Is -> f $ Y.Equal t'
        X.IsNot -> f $ Y.NotEqual t'
        X.In -> f $ Y.Elem t'
        X.NotIn -> Y.Lam [("x", t'), ("xs", Y.ListTy t')] (Y.Not' (Y.Elem' t' (Y.Var "x") (Y.Var "xs")))

makeList2 :: a -> a -> [a]
makeList2 x y = [x, y]

runExpr :: (MonadAlpha m, MonadError Error m) => X.Expr -> m Y.Expr
runExpr = \case
  X.BoolOp e1 op e2 -> Y.AppBuiltin (runBoolOp op) <$> (makeList2 <$> runExpr e1 <*> runExpr e2)
  X.BinOp e1 op e2 -> Y.AppBuiltin <$> runOperator op <*> (makeList2 <$> runExpr e1 <*> runExpr e2)
  X.UnaryOp op e -> Y.App (runUnaryOp op) . (: []) <$> runExpr e
  X.Lambda args body -> Y.Lam (map (runVarName *** runType) args) <$> runExpr body
  X.IfExp e1 e2 e3 -> do
    e1 <- runExpr e1
    e2 <- runExpr e2
    e3 <- runExpr e3
    t <- Y.genType
    return $ Y.AppBuiltin (Y.If t) [e1, e2, e3]
  X.ListComp _ (X.Comprehension _ _ _) -> undefined -- TODO
  X.Compare e1 op e2 -> Y.App (runCmpOp op) <$> (makeList2 <$> runExpr e1 <*> runExpr e2)
  X.Call f args -> Y.App <$> runExpr f <*> mapM runExpr args
  X.Constant const -> runConstant const
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
runToplevelStatements [] = return $ Y.ResultExpr (Y.Var "solve")
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
