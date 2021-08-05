{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Jikka.RestrictedPython.Convert.ToCore
-- Description : converts programs of our restricted Python to programs of core language. / 制限された Python のプログラムを core 言語のプログラムに変換します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Convert.ToCore
  ( run,
    runForStatement,
    runIfStatement,
  )
where

import Control.Monad.State.Strict
import Data.List (intersect, union)
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.Location
import qualified Jikka.Core.Language.BuiltinPatterns as Y
import qualified Jikka.Core.Language.Expr as Y
import qualified Jikka.Core.Language.Util as Y
import qualified Jikka.RestrictedPython.Language.Expr as X
import qualified Jikka.RestrictedPython.Language.Lint as X
import qualified Jikka.RestrictedPython.Language.Util as X
import qualified Jikka.RestrictedPython.Language.VariableAnalysis as X

type Env = [X.VarName]

defineVar :: MonadState Env m => X.VarName -> m ()
defineVar x = modify' (x :)

isDefinedVar :: MonadState Env m => X.VarName -> m Bool
isDefinedVar x = gets (x `elem`)

withScope :: MonadState Env m => m a -> m a
withScope f = do
  env <- get
  x <- f
  put env
  return x

runVarName :: X.VarName' -> Y.VarName
runVarName (X.WithLoc' _ (X.VarName x)) = Y.VarName x

runType :: MonadError Error m => X.Type -> m Y.Type
runType = \case
  X.VarTy (X.TypeName x) -> return $ Y.VarTy (Y.TypeName x)
  X.IntTy -> return Y.IntTy
  X.BoolTy -> return Y.BoolTy
  X.ListTy t -> Y.ListTy <$> runType t
  X.TupleTy ts -> Y.TupleTy <$> mapM runType ts
  X.CallableTy args ret -> Y.curryFunTy <$> mapM runType args <*> runType ret
  X.StringTy -> throwSemanticError "cannot use `str' type out of main function"
  X.SideEffectTy -> throwSemanticError "side-effect type must be used only as expr-statement" -- TODO: check in Jikka.RestrictedPython.Language.Lint

runConstant :: MonadError Error m => X.Constant -> m Y.Expr
runConstant = \case
  X.ConstNone -> return $ Y.Tuple' []
  X.ConstInt n -> return $ Y.Lit (Y.LitInt n)
  X.ConstBool p -> return $ Y.Lit (Y.LitBool p)
  X.ConstBuiltin builtin -> runBuiltin builtin

runBuiltin :: MonadError Error m => X.Builtin -> m Y.Expr
runBuiltin builtin = do
  let go0 builtin = do
        return $ Y.Lit (Y.LitBuiltin builtin [])
  let go1 builtin t1 = do
        t1 <- runType t1
        return $ Y.Lit (Y.LitBuiltin builtin [t1])
  let goN builtin ts = do
        ts <- mapM runType ts
        return $ Y.Lit (Y.LitBuiltin builtin ts)
  case builtin of
    X.BuiltinAbs -> go0 Y.Abs
    X.BuiltinPow -> go0 Y.Pow
    X.BuiltinModPow -> go0 Y.ModPow
    X.BuiltinDivMod -> return $ Y.Lam2 "a" Y.IntTy "b" Y.IntTy (Y.uncurryApp (Y.Tuple' [Y.IntTy, Y.IntTy]) [Y.FloorDiv' (Y.Var "a") (Y.Var "b"), Y.FloorMod' (Y.Var "a") (Y.Var "b")])
    X.BuiltinCeilDiv -> go0 Y.CeilDiv
    X.BuiltinCeilMod -> go0 Y.CeilMod
    X.BuiltinFloorDiv -> go0 Y.FloorDiv
    X.BuiltinFloorMod -> go0 Y.FloorMod
    X.BuiltinGcd -> go0 Y.Gcd
    X.BuiltinLcm -> go0 Y.Lcm
    X.BuiltinInt t -> case t of
      X.IntTy -> return $ Y.Lam "x" Y.IntTy (Y.Var "x")
      X.BoolTy -> return $ Y.Lam "p" Y.BoolTy (Y.If' Y.IntTy (Y.Var "p") Y.Lit1 Y.Lit0)
      _ -> throwTypeError "the argument of int must be int or bool"
    X.BuiltinBool t -> case t of
      X.IntTy -> return $ Y.Lam "x" Y.IntTy (Y.If' Y.BoolTy (Y.Equal' Y.IntTy (Y.Var "x") Y.Lit0) Y.LitFalse Y.LitTrue)
      X.BoolTy -> return $ Y.Lam "p" Y.BoolTy (Y.Var "p")
      X.ListTy t -> do
        t <- runType t
        return $ Y.Lam "xs" (Y.ListTy t) (Y.If' Y.BoolTy (Y.Equal' (Y.ListTy t) (Y.Var "xs") (Y.Lit (Y.LitNil t))) Y.LitFalse Y.LitTrue)
      _ -> throwTypeError "the argument of bool must be bool, int, or list(a)"
    X.BuiltinList t -> do
      t <- runType t
      return $ Y.Lam "xs" (Y.ListTy t) (Y.Var "xs")
    X.BuiltinTuple ts -> goN Y.Tuple ts
    X.BuiltinLen t -> go1 Y.Len t
    X.BuiltinMap ts ret -> case ts of
      [] -> Y.Nil' <$> runType ret
      _ -> do
        ts <- mapM runType ts
        ret <- runType ret
        let var i = Y.VarName ("xs" ++ show i)
        let lam body = Y.Lam "go0" (Y.curryFunTy ts ret) (foldr (\(i, t) -> Y.Lam (var i) (Y.ListTy t)) body (zip [0 ..] ts))
        let len = Y.Min1' Y.IntTy (foldr (Y.Cons' Y.IntTy) (Y.Nil' Y.IntTy) (zipWith (\i t -> Y.Len' t (Y.Var (var i))) [0 ..] ts))
        let body = Y.Map' Y.IntTy ret (Y.Lam "i" Y.IntTy (Y.uncurryApp (Y.Var "go0") (map (Y.Var . var) [0 .. length ts - 1]))) (Y.Range1' len)
        return $ lam body
    X.BuiltinSorted t -> go1 Y.Sorted t
    X.BuiltinReversed t -> go1 Y.Reversed t
    X.BuiltinEnumerate t -> do
      t <- runType t
      let body = Y.Lam "i" Y.IntTy (Y.uncurryApp (Y.Tuple' [Y.IntTy, t]) [Y.Var "i", Y.At' t (Y.Var "xs") (Y.Var "i")])
      return $ Y.Lam "xs" (Y.ListTy t) (Y.Map' (Y.ListTy t) (Y.ListTy (Y.TupleTy [Y.IntTy, t])) body (Y.Range1' (Y.Len' t (Y.Var "xs"))))
    X.BuiltinFilter t -> go1 Y.Filter t
    X.BuiltinZip ts -> do
      ts <- mapM runType ts
      let var i = Y.VarName ("xs" ++ show i)
      let lam body = foldr (\(i, t) -> Y.Lam (var i) (Y.ListTy t)) body (zip [0 ..] ts)
      let len = Y.Min1' Y.IntTy (foldr (Y.Cons' Y.IntTy) (Y.Nil' Y.IntTy) (zipWith (\i t -> Y.Len' t (Y.Var (var i))) [0 ..] ts))
      let body = Y.Map' Y.IntTy (Y.TupleTy ts) (Y.Lam "i" Y.IntTy (Y.uncurryApp (Y.Tuple' ts) (map (Y.Var . var) [0 .. length ts - 1]))) (Y.Range1' len)
      return $ lam body
    X.BuiltinAll -> go0 Y.All
    X.BuiltinAny -> go0 Y.Any
    X.BuiltinSum -> go0 Y.Sum
    X.BuiltinProduct -> go0 Y.Product
    X.BuiltinRange1 -> go0 Y.Range1
    X.BuiltinRange2 -> go0 Y.Range2
    X.BuiltinRange3 -> go0 Y.Range1
    X.BuiltinMax1 t -> go1 Y.Max1 t
    X.BuiltinMax t n -> do
      when (n < 2) $ do
        throwTypeError $ "max expected 2 or more arguments, got " ++ show n
      t <- runType t
      let args = map (\i -> Y.VarName ('x' : show i)) [0 .. n -1]
      return $ Y.curryLam (map (,t) args) (foldr1 (Y.Max2' t) (map Y.Var args))
    X.BuiltinMin1 t -> go1 Y.Min1 t
    X.BuiltinMin t n -> do
      when (n < 2) $ do
        throwTypeError $ "max min 2 or more arguments, got " ++ show n
      t <- runType t
      let args = map (\i -> Y.VarName ('x' : show i)) [0 .. n -1]
      return $ Y.curryLam (map (,t) args) (foldr1 (Y.Min2' t) (map Y.Var args))
    X.BuiltinArgMax t -> go1 Y.ArgMax t
    X.BuiltinArgMin t -> go1 Y.ArgMin t
    X.BuiltinFact -> go0 Y.Fact
    X.BuiltinChoose -> go0 Y.Choose
    X.BuiltinPermute -> go0 Y.Permute
    X.BuiltinMultiChoose -> go0 Y.MultiChoose
    X.BuiltinModInv -> go0 Y.ModInv
    X.BuiltinInput -> throwSemanticError "cannot use `input' out of main function"
    X.BuiltinPrint _ -> throwSemanticError "cannot use `print' out of main function"

runAttribute :: MonadError Error m => X.Attribute' -> m Y.Expr
runAttribute a = wrapAt' (loc' a) $ do
  case value' a of
    X.UnresolvedAttribute a -> throwInternalError $ "unresolved attribute: " ++ X.unAttributeName a
    X.BuiltinCount t -> do
      t <- runType t
      return $ Y.Lam2 "xs" (Y.ListTy t) "x" t (Y.Len' t (Y.Filter' t (Y.Lam "y" t (Y.Equal' t (Y.Var "x") (Y.Var "y"))) (Y.Var "xs")))
    X.BuiltinIndex t -> do
      t <- runType t
      return $ Y.Lam2 "xs" (Y.ListTy t) "x" t (Y.Min1' Y.IntTy (Y.Filter' Y.IntTy (Y.Lam "i" Y.IntTy (Y.Equal' t (Y.At' t (Y.Var "xs") (Y.Var "i")) (Y.Var "x"))) (Y.Range1' (Y.Len' t (Y.Var "xs")))))
    X.BuiltinCopy t -> do
      t <- runType t
      return $ Y.Lam "x" t (Y.Var "x")
    X.BuiltinAppend _ -> throwSemanticError "cannot use `append' out of expr-statements"
    X.BuiltinSplit -> throwSemanticError "cannot use `split' out of main function"

runBoolOp :: X.BoolOp -> Y.Builtin
runBoolOp = \case
  X.And -> Y.And
  X.Or -> Y.Or
  X.Implies -> Y.Implies

runUnaryOp :: X.UnaryOp -> Y.Expr
runUnaryOp = \case
  X.Invert -> Y.Builtin Y.BitNot
  X.Not -> Y.Builtin Y.Not
  X.UAdd -> Y.Lam "x" Y.IntTy (Y.Var "x")
  X.USub -> Y.Builtin Y.Negate

runOperator :: MonadError Error m => X.Operator -> m (Y.Builtin, [Y.Type])
runOperator op = do
  let go0 builtin = return (builtin, [])
  let go1 builtin t1 = return (builtin, [t1])
  case op of
    X.Add -> go0 Y.Plus
    X.Sub -> go0 Y.Minus
    X.Mult -> go0 Y.Mult
    X.MatMult -> throwSemanticError "matmul operator ('@') is not supported"
    X.Div -> throwSemanticError "floatdiv operator ('/') is not supported"
    X.FloorDiv -> go0 Y.FloorDiv
    X.FloorMod -> go0 Y.FloorMod
    X.CeilDiv -> go0 Y.CeilDiv
    X.CeilMod -> go0 Y.CeilMod
    X.Pow -> go0 Y.Pow
    X.BitLShift -> go0 Y.BitLeftShift
    X.BitRShift -> go0 Y.BitRightShift
    X.BitOr -> go0 Y.BitOr
    X.BitXor -> go0 Y.BitXor
    X.BitAnd -> go0 Y.BitAnd
    X.Max -> go1 Y.Max2 Y.IntTy
    X.Min -> go1 Y.Min2 Y.IntTy

runCmpOp :: MonadError Error m => X.CmpOp' -> m Y.Expr
runCmpOp (X.CmpOp' op t) = do
  t <- runType t
  let go1 builtin t1 = Y.Builtin1 builtin t1
  return $ case op of
    X.Lt -> go1 Y.LessThan t
    X.LtE -> go1 Y.LessEqual t
    X.Gt -> go1 Y.GreaterThan t
    X.GtE -> go1 Y.GreaterEqual t
    X.Eq' -> go1 Y.Equal t
    X.NotEq -> go1 Y.NotEqual t
    X.Is -> go1 Y.Equal t
    X.IsNot -> go1 Y.NotEqual t
    X.In -> go1 Y.Elem t
    X.NotIn -> Y.curryLam [("x", t), ("xs", Y.ListTy t)] (Y.Not' (Y.Elem' t (Y.Var "x") (Y.Var "xs")))

runTargetExpr :: (MonadAlpha m, MonadError Error m) => X.Target' -> m Y.Expr
runTargetExpr (WithLoc' _ x) = case x of
  X.SubscriptTrg x e -> Y.At' <$> Y.genType <*> runTargetExpr x <*> runExpr e
  X.NameTrg x -> return $ Y.Var (runVarName x)
  X.TupleTrg xs -> Y.uncurryApp <$> (Y.Tuple' <$> replicateM (length xs) Y.genType) <*> mapM runTargetExpr xs

runAssign :: (MonadAlpha m, MonadError Error m) => X.Target' -> Y.Expr -> m Y.Expr -> m Y.Expr
runAssign (WithLoc' _ x) e cont = case x of
  X.SubscriptTrg x index -> join $ runAssign x <$> (Y.SetAt' <$> Y.genType <*> runTargetExpr x <*> runExpr index <*> pure e) <*> pure cont
  X.NameTrg x -> Y.Let (runVarName x) <$> Y.genType <*> pure e <*> cont
  X.TupleTrg xs -> do
    y <- Y.genVarName'
    ts <- replicateM (length xs) Y.genType
    cont <- join $ foldM (\cont (i, x) -> return $ runAssign x (Y.Proj' ts i (Y.Var y)) cont) cont (zip [0 ..] xs)
    return $ Y.Let y (Y.TupleTy ts) e cont

runListComp :: (MonadAlpha m, MonadError Error m) => X.Expr' -> X.Comprehension -> m Y.Expr
runListComp e (X.Comprehension x iter pred) = do
  iter <- runExpr iter
  y <- Y.genVarName'
  t1 <- Y.genType
  iter <- case pred of
    Nothing -> return iter
    Just pred -> Y.Filter' t1 <$> (Y.Lam y t1 <$> runAssign x (Y.Var y) (runExpr pred)) <*> pure iter
  t2 <- Y.genType
  e <- runExpr e
  Y.Map' t1 t2 <$> (Y.Lam y t1 <$> runAssign x (Y.Var y) (pure e)) <*> pure iter

runExpr :: (MonadAlpha m, MonadError Error m) => X.Expr' -> m Y.Expr
runExpr e0 = wrapAt' (loc' e0) $ case value' e0 of
  X.BoolOp e1 op e2 -> Y.AppBuiltin2 (runBoolOp op) <$> runExpr e1 <*> runExpr e2
  X.BinOp e1 op e2 -> Y.App2 <$> (Y.Lit <$> (uncurry Y.LitBuiltin <$> runOperator op)) <*> runExpr e1 <*> runExpr e2
  X.UnaryOp op e -> Y.App (runUnaryOp op) <$> runExpr e
  X.Lambda args body -> Y.curryLam <$> mapM (\(x, t) -> (runVarName x,) <$> runType t) args <*> runExpr body
  X.IfExp e1 e2 e3 -> do
    e1 <- runExpr e1
    e2 <- runExpr e2
    e3 <- runExpr e3
    t <- Y.genType
    return $ Y.If' t e1 e2 e3
  X.ListComp x comp -> runListComp x comp
  X.Compare e1 op e2 -> Y.App2 <$> runCmpOp op <*> runExpr e1 <*> runExpr e2
  X.Call f args -> Y.uncurryApp <$> runExpr f <*> mapM runExpr args
  X.Constant const -> runConstant const
  X.Attribute e a -> do
    e <- runExpr e
    a <- runAttribute a
    return $ Y.App a e
  X.Subscript e1 e2 -> Y.App2 <$> (Y.Builtin1 Y.At <$> Y.genType) <*> runExpr e1 <*> runExpr e2
  X.Starred e -> throwSemanticErrorAt' (loc' e) "cannot use starred expr"
  X.Name x -> return $ Y.Var (runVarName x)
  X.List t es -> do
    t <- runType t
    foldr (Y.Cons' t) (Y.Lit (Y.LitNil t)) <$> mapM runExpr es
  X.Tuple es -> Y.uncurryApp <$> (Y.Tuple' <$> mapM (const Y.genType) es) <*> mapM runExpr es
  X.SubscriptSlice e from to step -> do
    e <- runExpr e
    from <- traverse runExpr from
    to <- traverse runExpr to
    step <- traverse runExpr step
    i <- Y.genVarName'
    t <- Y.genType
    let mapAt = return . Y.Map' Y.IntTy t (Y.Lam i t (Y.At' t e (Y.Var i)))
    case (from, to, step) of
      (Nothing, Nothing, Nothing) -> return e
      (Nothing, Just to, Nothing) -> mapAt (Y.Range1' to)
      (Just from, Nothing, Nothing) -> mapAt (Y.Range2' from (Y.Len' t e))
      (Just from, Just to, Nothing) -> mapAt (Y.Range2' from to)
      (Nothing, Nothing, Just step) -> mapAt (Y.Range3' Y.Lit0 (Y.Len' t e) step)
      (Nothing, Just to, Just step) -> mapAt (Y.Range3' Y.Lit0 to step)
      (Just from, Nothing, Just step) -> mapAt (Y.Range3' from (Y.Len' t e) step)
      (Just from, Just to, Just step) -> mapAt (Y.Range3' from to step)

-- | `runForStatement` converts for-loops to `foldl`.
-- For example, this converts the following:
--
-- > # a, b are defined
-- > for _ in range(n):
-- >     c = a + b
-- >     a = b
-- >     b = c
-- > ...
--
-- to:
--
-- > let (a, b) = foldl (fun (a, b) i -> (b, a + b)) (a, b) (range n)
-- > in ...
runForStatement :: (MonadState Env m, MonadAlpha m, MonadError Error m) => X.Target' -> X.Expr' -> [X.Statement] -> [X.Statement] -> [[X.Statement]] -> m Y.Expr
runForStatement x iter body cont conts = do
  tx <- Y.genType
  iter <- runExpr iter
  x' <- Y.genVarName'
  z <- Y.genVarName'
  let (_, X.WriteList w) = X.analyzeStatementsMax body
  ys <- filterM isDefinedVar w
  ts <- replicateM (length ys) Y.genType
  let init = Y.uncurryApp (Y.Tuple' ts) (map (Y.Var . runVarName . withoutLoc) ys)
  let write cont = foldr (\(i, y, t) -> Y.Let (runVarName $ X.WithLoc' Nothing y) t (Y.Proj' ts i (Y.Var z))) cont (zip3 [0 ..] ys ts)
  body <- runAssign x (Y.Var x') $ do
    runStatements (body ++ [X.Return (withoutLoc (X.Tuple (map (withoutLoc . X.Name . withoutLoc) ys)))]) (cont : conts)
  let loop init = Y.Foldl' tx (Y.TupleTy ts) (Y.Lam2 z (Y.TupleTy ts) x' tx (write body)) init iter
  cont <- runStatements cont conts
  return $ Y.Let z (Y.TupleTy ts) (loop init) (write cont)

-- | `runIfStatement` converts if-loops to if-exprs.
--
-- > # a, b are defined
-- > if True:
-- >     a = 0
-- >     b = 1
-- >     c = 3
-- > else:
-- >     a = 1
-- >     c = 10
-- > ...
--
-- to:
--
-- > let (a, c) = if true then (0, 3) else (1, 10)
-- > in ...
runIfStatement :: (MonadState Env m, MonadAlpha m, MonadError Error m) => X.Expr' -> [X.Statement] -> [X.Statement] -> [X.Statement] -> [[X.Statement]] -> m Y.Expr
runIfStatement e body1 body2 cont conts = do
  e <- runExpr e
  t <- Y.genType
  case (any X.doesAlwaysReturn body1, any X.doesAlwaysReturn body2) of
    (False, False) -> do
      let (_, X.WriteList w1) = X.analyzeStatementsMin body1
      let (_, X.WriteList w2) = X.analyzeStatementsMin body2
      let (X.ReadList r, _) = X.analyzeStatementsMax (concat (cont : conts))
      let w = (r `intersect` w1) `union` (r `intersect` w2)
      let read = withoutLoc (X.Tuple (map (withoutLoc . X.Name . withoutLoc) w))
      ts <- replicateM (length w) Y.genType
      z <- Y.genVarName'
      let write value cont = Y.Let z (Y.TupleTy ts) value (foldr (\(i, y, t) -> Y.Let (runVarName (withoutLoc y)) t (Y.Proj' ts i (Y.Var z))) cont (zip3 [0 ..] w ts))
      body1 <- runStatements (body1 ++ [X.Return read]) (cont : conts)
      body2 <- runStatements (body2 ++ [X.Return read]) (cont : conts)
      cont <- runStatements cont conts
      return $ write (Y.If' t e body1 body2) cont
    (False, True) -> Y.If' t e <$> runStatements (body1 ++ cont) conts <*> runStatements body2 []
    (True, False) -> Y.If' t e <$> runStatements body1 [] <*> runStatements (body2 ++ cont) conts
    (True, True) -> Y.If' t e <$> runStatements body1 [] <*> runStatements body2 []

runStatements :: (MonadState Env m, MonadAlpha m, MonadError Error m) => [X.Statement] -> [[X.Statement]] -> m Y.Expr
runStatements [] _ = throwSemanticError "function may not return"
runStatements (stmt : stmts) cont = case stmt of
  X.Return e -> runExpr e
  X.AugAssign x op e -> do
    y <- runTargetExpr x
    op <- Y.Lit . uncurry Y.LitBuiltin <$> runOperator op
    e <- runExpr e
    runAssign x (Y.App2 op y e) $ do
      runStatements stmts cont
  X.AnnAssign x _ e -> do
    e <- runExpr e
    runAssign x e $ do
      withScope $ do
        mapM_ defineVar (X.targetVars x)
        runStatements stmts cont
  X.For x iter body -> runForStatement x iter body stmts cont
  X.If e body1 body2 -> runIfStatement e body1 body2 stmts cont
  X.Assert e -> do
    e <- runExpr e
    cont <- runStatements stmts cont
    return $ Y.Assert e cont
  X.Append loc t x e -> do
    case X.exprToTarget x of
      Nothing -> throwSemanticErrorAt' loc "invalid `append` method"
      Just x -> do
        t <- runType t
        y <- runTargetExpr x
        e <- runExpr e
        runAssign x (Y.Snoc' t y e) $ do
          runStatements stmts cont
  X.Expr' e -> throwSemanticErrorAt' (loc' e) "invalid expr-statement"

runToplevelStatements :: (MonadState Env m, MonadAlpha m, MonadError Error m) => [X.ToplevelStatement] -> m Y.ToplevelExpr
runToplevelStatements [] = return $ Y.ResultExpr (Y.Var "solve")
runToplevelStatements (stmt : stmts) = case stmt of
  X.ToplevelAnnAssign x t e -> do
    e <- runExpr e
    defineVar (X.value' x)
    cont <- runToplevelStatements stmts
    t <- runType t
    return $ Y.ToplevelLet (runVarName x) t e cont
  X.ToplevelFunctionDef f args ret body -> do
    defineVar (X.value' f)
    body <- withScope $ do
      mapM_ (defineVar . X.value' . fst) args
      runStatements body []
    cont <- runToplevelStatements stmts
    args <- mapM (\(x, t) -> (runVarName x,) <$> runType t) args
    ret <- runType ret
    return $ Y.ToplevelLetRec (runVarName f) args ret body cont
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
--
-- For example, this converts the following:
--
-- > def solve(n):
-- >     if n == 0:
-- >         return 1
-- >     else:
-- >         return n * solve(n - 1)
--
-- to:
--
-- > let solve n =
-- >     if n == 0 then
-- >         1
-- >     else:
-- >         n * solve (n - 1)
-- > in solve
--
-- Also, this converts the following:
--
-- > def solve(n):
-- >     a = 0
-- >     b = 1
-- >     for _ in range(n):
-- >         c = a + b
-- >         a = b
-- >         b = c
-- >     return a
--
-- to:
--
-- > let solve n =
-- >     fst (foldl (fun (a, b) i -> (b, a + b)) (0, 1) [0 .. n - 1])
-- > in solve
run :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
run prog = wrapError' "Jikka.RestrictedPython.Convert.ToCore" $ do
  X.ensureDoesntHaveSubscriptionInLoopCounters prog
  X.ensureDoesntHaveLeakOfLoopCounters prog
  X.ensureDoesntHaveAssignmentToLoopCounters prog
  X.ensureDoesntHaveAssignmentToLoopIterators prog
  X.ensureDoesntHaveReturnInLoops prog
  X.ensureDoesntHaveNonTrivialSubscriptedAssignmentInForLoops prog
  evalStateT (runToplevelStatements prog) []
