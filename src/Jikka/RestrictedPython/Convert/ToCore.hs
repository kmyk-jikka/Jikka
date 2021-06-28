{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Jikka.RestrictedPython.Convert.ToCore
  ( run,
    runForStatement,
    runIfStatement,
  )
where

import Control.Arrow ((***))
import Control.Monad.State.Strict
import Data.List (intersect)
import Jikka.Common.Alpha
import Jikka.Common.Error
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
  X.ConstNone -> return $ Y.Tuple' [] []
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
        X.BuiltinDivMod -> return $ Y.Lam [("a", Y.IntTy), ("b", Y.IntTy)] (Y.Tuple' [Y.IntTy, Y.IntTy] [Y.FloorDiv' (Y.Var "a") (Y.Var "b"), Y.FloorMod' (Y.Var "a") (Y.Var "b")])
        X.BuiltinCeilDiv -> f Y.CeilDiv
        X.BuiltinCeilMod -> f Y.CeilMod
        X.BuiltinFloorDiv -> f Y.FloorDiv
        X.BuiltinFloorMod -> f Y.FloorMod
        X.BuiltinGcd -> f Y.Gcd
        X.BuiltinLcm -> f Y.Lcm
        X.BuiltinInt t -> case t of
          X.IntTy -> return $ Y.Lam [("x", Y.IntTy)] (Y.Var "x")
          X.BoolTy -> return $ Y.Lam [("p", Y.BoolTy)] (Y.If' Y.IntTy (Y.Var "p") Y.Lit1 Y.Lit0)
          _ -> throwTypeError "the argument of int must be int or bool"
        X.BuiltinBool t -> case t of
          X.IntTy -> return $ Y.Lam [("x", Y.IntTy)] (Y.If' Y.BoolTy (Y.Equal' Y.IntTy (Y.Var "x") Y.Lit0) Y.LitFalse Y.LitTrue)
          X.BoolTy -> return $ Y.Lam [("p", Y.BoolTy)] (Y.Var "p")
          X.ListTy t ->
            let t' = runType t
             in return $ Y.Lam [("xs", Y.ListTy t')] (Y.If' Y.BoolTy (Y.Equal' (Y.ListTy t') (Y.Var "xs") (Y.Lit (Y.LitNil t'))) Y.LitFalse Y.LitTrue)
          _ -> throwTypeError "the argument of bool must be bool, int, or list(a)"
        X.BuiltinList t -> return $ Y.Lam [("xs", Y.ListTy (runType t))] (Y.Var "xs")
        X.BuiltinTuple ts -> f $ Y.Tuple (map runType ts)
        X.BuiltinLen t -> f $ Y.Len (runType t)
        X.BuiltinMap _ _ -> throwInternalError "runBuiltin TODO"
        X.BuiltinSorted t -> f $ Y.Sorted (runType t)
        X.BuiltinReversed t -> f $ Y.Reversed (runType t)
        X.BuiltinEnumerate _ -> throwInternalError "runBuiltin TODO"
        X.BuiltinFilter _ -> throwInternalError "runBuiltin TODO"
        X.BuiltinZip _ -> throwInternalError "runBuiltin TODO"
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

runTargetExpr :: (MonadAlpha m, MonadError Error m) => X.Target -> m Y.Expr
runTargetExpr = \case
  X.SubscriptTrg x e -> Y.At' <$> Y.genType <*> runTargetExpr x <*> runExpr e
  X.NameTrg x -> return $ Y.Var (runVarName x)
  X.TupleTrg xs -> Y.Tuple' <$> replicateM (length xs) Y.genType <*> mapM runTargetExpr xs

runAssign :: (MonadAlpha m, MonadError Error m) => X.Target -> Y.Expr -> m Y.Expr -> m Y.Expr
runAssign x e cont = case x of
  X.SubscriptTrg x index -> join $ runAssign x <$> (Y.SetAt' <$> Y.genType <*> runTargetExpr x <*> runExpr index <*> pure e) <*> pure cont
  X.NameTrg x -> Y.Let (runVarName x) <$> Y.genType <*> pure e <*> cont
  X.TupleTrg xs -> do
    y <- Y.genVarName'
    ts <- replicateM (length xs) Y.genType
    cont <- join $ foldM (\cont (i, x) -> return $ runAssign x (Y.Proj' ts i (Y.Var y)) cont) cont (zip [0 ..] xs)
    return $ Y.Let y (Y.TupleTy ts) e cont

runListComp :: (MonadAlpha m, MonadError Error m) => X.Expr -> X.Comprehension -> m Y.Expr
runListComp e (X.Comprehension x iter pred) = do
  iter <- runExpr iter
  iter <- case pred of
    Nothing -> return iter
    Just pred -> Y.Filter' <$> Y.genType <*> runExpr pred <*> pure iter
  y <- Y.genVarName'
  t1 <- Y.genType
  t2 <- Y.genType
  e <- runExpr e
  Y.Map' t1 t2 <$> (Y.Lam [(y, t1)] <$> runAssign x (Y.Var y) (pure e)) <*> pure iter

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
  X.ListComp x comp -> runListComp x comp
  X.Compare e1 op e2 -> Y.App (runCmpOp op) <$> (makeList2 <$> runExpr e1 <*> runExpr e2)
  X.Call f args -> Y.App <$> runExpr f <*> mapM runExpr args
  X.Constant const -> runConstant const
  X.Subscript e1 e2 -> Y.AppBuiltin <$> (Y.At <$> Y.genType) <*> (makeList2 <$> runExpr e1 <*> runExpr e2)
  X.Name x -> return $ Y.Var (runVarName x)
  X.List t es -> do
    let t' = runType t
    foldr (Y.Cons' t') (Y.Lit (Y.LitNil t')) <$> mapM runExpr es
  X.Tuple es -> Y.Tuple' <$> mapM (const Y.genType) es <*> mapM runExpr es
  X.SubscriptSlice _ _ _ _ -> throwInternalError "runExpr TODO"

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
runForStatement :: (MonadState Env m, MonadAlpha m, MonadError Error m) => X.Target -> X.Expr -> [X.Statement] -> [X.Statement] -> m Y.Expr
runForStatement x iter body cont = do
  tx <- Y.genType
  iter <- runExpr iter
  x' <- Y.genVarName'
  z <- Y.genVarName'
  let (_, X.WriteList w) = X.analyzeStatementsMax body
  ys <- filterM isDefinedVar w
  ts <- replicateM (length ys) Y.genType
  let init = Y.Tuple' ts (map (Y.Var . runVarName . X.WithLoc' Nothing) ys)
  let write cont = foldr (\(i, y, t) -> Y.Let (runVarName $ X.WithLoc' Nothing y) t (Y.Proj' ts i (Y.Var z))) cont (zip3 [0 ..] ys ts)
  body <- runAssign x (Y.Var x') $ do
    runStatements (body ++ [X.Return (X.Tuple (map (X.Name . X.WithLoc' Nothing) ys))])
  let loop init = Y.Foldl' tx (Y.TupleTy ts) (Y.Lam [(z, Y.TupleTy ts), (x', tx)] (write body)) init iter
  cont <- runStatements cont
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
runIfStatement :: (MonadState Env m, MonadAlpha m, MonadError Error m) => X.Expr -> [X.Statement] -> [X.Statement] -> [X.Statement] -> m Y.Expr
runIfStatement e body1 body2 cont = do
  e <- runExpr e
  t <- Y.genType
  case (any X.doesAlwaysReturn body1, any X.doesAlwaysReturn body2) of
    (False, False) -> do
      let (_, X.WriteList w1) = X.analyzeStatementsMin body1
      let (_, X.WriteList w2) = X.analyzeStatementsMin body2
      let w = w1 `intersect` w2
      let read = X.Tuple (map (X.Name . X.WithLoc' Nothing) w)
      ts <- replicateM (length w) Y.genType
      z <- Y.genVarName'
      let write value cont = Y.Let z (Y.TupleTy ts) value (foldr (\(i, y, t) -> Y.Let (runVarName (X.WithLoc' Nothing y)) t (Y.Proj' ts i (Y.Var z))) cont (zip3 [0 ..] w ts))
      body1 <- runStatements (body1 ++ [X.Return read])
      body2 <- runStatements (body2 ++ [X.Return read])
      cont <- runStatements cont
      return $ write (Y.AppBuiltin (Y.If t) [e, body1, body2]) cont
    (False, True) -> Y.If' t e <$> runStatements (body1 ++ cont) <*> runStatements body2
    (True, False) -> Y.If' t e <$> runStatements body1 <*> runStatements (body2 ++ cont)
    (True, True) -> Y.If' t e <$> runStatements body1 <*> runStatements body2

runStatements :: (MonadState Env m, MonadAlpha m, MonadError Error m) => [X.Statement] -> m Y.Expr
runStatements [] = throwSemanticError "function may not return"
runStatements (stmt : stmts) = case stmt of
  X.Return e -> runExpr e
  X.AugAssign x op e -> do
    y <- runTargetExpr x
    op <- Y.Lit . Y.LitBuiltin <$> runOperator op
    e <- runExpr e
    runAssign x (Y.App op [y, e]) $ do
      runStatements stmts
  X.AnnAssign x _ e -> do
    e <- runExpr e
    runAssign x e $ do
      withScope $ do
        mapM_ defineVar (X.targetVars x)
        runStatements stmts
  X.For x iter body -> runForStatement x iter body stmts
  X.If e body1 body2 -> runIfStatement e body1 body2 stmts
  X.Assert _ -> runStatements stmts

runToplevelStatements :: (MonadState Env m, MonadAlpha m, MonadError Error m) => [X.ToplevelStatement] -> m Y.ToplevelExpr
runToplevelStatements [] = return $ Y.ResultExpr (Y.Var "solve")
runToplevelStatements (stmt : stmts) = case stmt of
  X.ToplevelAnnAssign x t e -> do
    e <- runExpr e
    defineVar (X.value' x)
    cont <- runToplevelStatements stmts
    return $ Y.ToplevelLet (runVarName x) (runType t) e cont
  X.ToplevelFunctionDef f args ret body -> do
    defineVar (X.value' f)
    body <- withScope $ do
      mapM_ (defineVar . X.value' . fst) args
      runStatements body
    cont <- runToplevelStatements stmts
    return $ Y.ToplevelLetRec (runVarName f) (map (runVarName *** runType) args) (runType ret) body cont
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
