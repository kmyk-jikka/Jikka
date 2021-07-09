{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.RestrictedPython.Evaluate
-- Description : evaluates programs of the restricted Python. / 制限された Python のプログラムを評価します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Evaluate
  ( run,
    makeGlobal,
    runWithGlobal,
    evalExpr,
    evalStatement,
    evalStatements,
    execToplevelStatement,
  )
where

import Control.Arrow (first)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bits
import Data.List (maximumBy, minimumBy, sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Jikka.Common.Combinatorics
import Jikka.Common.Error
import Jikka.RestrictedPython.Format (formatBuiltin, formatOperator)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Lint
import Jikka.RestrictedPython.Language.Value

assign :: MonadState Local m => VarName -> Value -> m ()
assign x v = modify' (Local . M.insert x v . unLocal)

lookupLocal :: (MonadState Local m, MonadError Error m) => VarName' -> m Value
lookupLocal x = do
  local <- get
  case M.lookup (value' x) (unLocal local) of
    Just v -> return v
    Nothing -> maybe id wrapAt (loc' x) . throwInternalError $ "undefined variable: " ++ unVarName (value' x)

assignSubscriptedTarget :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Target' -> Expr' -> Value -> m ()
assignSubscriptedTarget f index v = maybe id wrapAt (loc' f) $ do
  let go f indices = maybe id wrapAt (loc' f) $ case value' f of
        SubscriptTrg f index -> go f (index : indices)
        NameTrg x -> return (x, indices)
        TupleTrg _ -> throwInternalError "cannot subscript a tuple target"
  (x, indices) <- go f [index]
  f <- lookupLocal x
  indices <- mapM evalExpr indices
  let go f index = case (f, index) of
        (_, []) -> return v
        (ListVal f, IntVal index : indices) -> do
          when (index < 0 || fromIntegral (V.length f) <= index) $ do
            throwRuntimeError "list index out of range"
          v' <- go (f V.! fromInteger index) indices
          return $ ListVal (f V.// [(fromInteger index, v')])
        (_, _) -> throwInternalError "type error"
  f <- go f indices
  assign (value' x) f

assignTarget :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Target' -> Value -> m ()
assignTarget x0 v = maybe id wrapAt (loc' x0) $ case value' x0 of
  SubscriptTrg f index -> do
    assignSubscriptedTarget f index v
  NameTrg x -> do
    assign (value' x) v
  TupleTrg xs -> do
    case v of
      TupleVal vs -> do
        when (length xs /= length vs) $ do
          throwInternalError "the lengths of tuple are different"
        forM_ (zip xs vs) $ \(x, v) -> do
          assignTarget x v
      _ -> throwInternalError "cannot unpack non-tuple value"

evalTarget :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Target' -> m Value
evalTarget x0 = maybe id wrapAt (loc' x0) $ case value' x0 of
  SubscriptTrg f index -> do
    f <- evalTarget f
    index <- evalExpr index
    case (f, index) of
      (ListVal f, IntVal index) -> do
        when (index < 0 || fromIntegral (V.length f) <= index) $ do
          throwRuntimeError "list index out of range"
        return $ f V.! fromInteger index
      (_, _) -> throwInternalError "type error"
  NameTrg x -> lookupLocal x
  TupleTrg xs -> TupleVal <$> mapM evalTarget xs

-- | `evalExpr` evaluates exprs of our restricted Python-like language.
--
-- === Rules for \(e_1 \operatorname{boolop} e_2\)
--
-- \[
--     \cfrac{e_1 \mid \mu \Downarrow \mathbf{false}}{e_1 ~\mathbf{and}~ e_2 \mid \mu \Downarrow \mathbf{false}}
-- \]
-- \[
--     \cfrac{e_1 \mid \mu \Downarrow \mathbf{true} \qquad e_2 \mid \mu \Downarrow v}{e_1 ~\mathbf{and}~ e_2 \mid \mu \Downarrow v}
-- \]
-- \[
--     \vdots
-- \]
--
-- === Rules for \(e_1 \operatorname{binop} e_2\)
--
-- \[
--     \cfrac{e_1 \mid \mu \Downarrow v_1 \qquad e_2 \mid \mu \Downarrow v_2}{e_1 + e_2 \mid \mu \Downarrow (v_1 + v_2)}
-- \]
-- \[
--     \vdots
-- \]
--
-- === Rules for \(\operatorname{unaryop} e\)
--
-- === Rules for \(\lambda x _ \tau x _ \tau \dots x _ \tau. e\)
--
-- \[
--     \lambda {x_0} _ {\tau _ 0} {x_1} _ {\tau _ 1} \dots {x _ {n - 1}} _ {\tau _ {n - 1}}. e \mid \mu \Downarrow \lambda _ {\mu} x_0 x_1 \dots x _ {n - 1}. e
-- \]
--
-- === Rules for \(\mathbf{if}~ e ~\mathbf{then}~ e ~\mathbf{else}~ e\)
--
-- === Rules for \(\lbrack e ~\mathbf{for}~ y ~\mathbf{in}~ e ~(\mathbf{if}~ e)? \rbrack\)
--
-- === Rules for \(e \operatorname{cmpop} e\)
--
-- === Rules for \(e (e, e, \dots, e)\)
--
-- \[
--     \cfrac{
--         e \mid \mu \Downarrow \lambda _ {\mu'} x_0 x_1 \dots x _ {n - 1}. e'
--         \qquad e_0 \mid \mu \Downarrow v_0
--         \qquad e_1 \mid \mu \Downarrow v_1
--         \qquad \dots
--         \qquad e _ {n - 1} \mid \mu \Downarrow v _ {n - 1}
--         \qquad e' \mid (x_0 \mapsto v_0; x_1 \mapsto v_1; \dots; x _ {n - 1} \mapsto v _ {n - 1}; \mu') \Downarrow v
--     }{
--         e(e_0, e_1, \dots, e _ {n - 1}) \mid \mu \Downarrow v
--     }
-- \]
--
-- \[
--     \cfrac{
--         e \mid \mu \Downarrow b
--         \qquad e_0 \mid \mu \Downarrow v_0
--         \qquad e_1 \mid \mu \Downarrow v_1
--         \qquad \dots
--         \qquad e _ {n - 1} \mid \mu \Downarrow v _ {n - 1}
--     }{
--         e(e_0, e_1, \dots, e _ {n - 1}) \mid \mu \Downarrow b(e_0, e_1, \dots, e _ {n - 1})
--     }
--     \qquad{(b ~\text{is a builtin function})}
-- \]
--
-- === Rules for \(\operatorname{constant}\)
--
-- === Rules for \(e \lbrack e \rbrack\)
--
-- === Rules for \(x\)
--
-- \[
--     x \mid \mu \Downarrow \mu(x)
-- \]
--
-- === Rules for \(\lbrack e, e, \dots, e \rbrack _ \tau\)
--
-- === Rules for \(e \lbrack e? \colon e? \colon e? \rbrack\)
evalExpr :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Expr' -> m Value
evalExpr e0 = maybe id wrapAt (loc' e0) $ case value' e0 of
  BoolOp e1 op e2 -> do
    v1 <- evalExpr e1
    case (v1, op) of
      (BoolVal False, And) -> return $ BoolVal False
      (BoolVal True, And) -> evalExpr e2
      (BoolVal False, Or) -> evalExpr e2
      (BoolVal True, Or) -> return $ BoolVal True
      (BoolVal False, Implies) -> return $ BoolVal True
      (BoolVal True, Implies) -> evalExpr e2
      (_, _) -> throwInternalError "type error"
  BinOp e1 op e2 -> do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    evalBinOp v1 op v2
  UnaryOp op e -> do
    v <- evalExpr e
    case (op, v) of
      (Invert, IntVal v) -> return $ IntVal (complement v)
      (Not, BoolVal v) -> return $ BoolVal (not v)
      (UAdd, IntVal v) -> return $ IntVal v
      (USub, IntVal v) -> return $ IntVal (- v)
      (_, _) -> throwInternalError "type error"
  Lambda args body -> do
    savedLocal <- get
    return $ ClosureVal savedLocal (map (first value') args) [Return body]
  IfExp e1 e2 e3 -> do
    v1 <- evalExpr e1
    case v1 of
      BoolVal True -> evalExpr e2
      BoolVal False -> evalExpr e3
      _ -> throwInternalError "type error"
  ListComp e (Comprehension x iter pred) -> do
    iter <- evalExpr iter
    case iter of
      ListVal iter -> do
        savedLocal <- get
        vs <- V.forM iter $ \it -> do
          assignTarget x it
          pred <- mapM evalExpr pred
          case pred of
            Just (BoolVal False) -> return Nothing
            _ -> Just <$> evalExpr e
        put savedLocal
        return $ ListVal (V.catMaybes vs)
      _ -> throwInternalError "type error"
  Compare e1 op e2 -> do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case op of
      CmpOp' In _ -> do
        v2 <- toList v2
        return $ BoolVal (v1 `V.elem` v2)
      CmpOp' NotIn _ -> do
        v2 <- toList v2
        return $ BoolVal (v1 `V.elem` v2)
      CmpOp' op _ -> do
        ordering <- maybe (throwInternalError "something wrong") return (compareValues v1 v2)
        BoolVal <$> case op of
          Eq' -> return $ ordering == EQ
          NotEq -> return $ ordering /= EQ
          Lt -> return $ ordering == LT
          LtE -> return $ ordering /= GT
          Gt -> return $ ordering == GT
          GtE -> return $ ordering /= LT
          Is -> return $ ordering == EQ
          IsNot -> return $ ordering /= EQ
          _ -> throwInternalError "something wrong"
  Call f args -> evalCall f args
  Constant const ->
    return $ case const of
      ConstNone -> TupleVal []
      ConstInt v -> IntVal v
      ConstBool v -> BoolVal v
      ConstBuiltin v -> BuiltinVal v
  Subscript e1 e2 -> do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
      (ListVal v1, IntVal v2) -> do
        when (v2 < 0 || fromIntegral (V.length v1) <= v2) $ do
          throwRuntimeError "list index out of range"
        return $ v1 V.! fromInteger v2
      _ -> throwInternalError "type error"
  Name x -> do
    local <- get
    case M.lookup (value' x) (unLocal local) of
      Just v -> return v
      Nothing -> do
        global <- ask
        case M.lookup (value' x) (unGlobal global) of
          Just v -> return v
          Nothing -> throwInternalError $ "undefined variable: " ++ unVarName (value' x)
  List _ es -> ListVal . V.fromList <$> mapM evalExpr es
  Tuple es -> TupleVal <$> mapM evalExpr es
  SubscriptSlice e from to step -> do
    v <- evalExpr e
    from <- mapM evalExpr from
    to <- mapM evalExpr to
    step <- mapM evalExpr step
    case v of
      ListVal v ->
        ListVal <$> case (from, to, step) of
          (_, _, Just _) -> throwInternalError "slice with step is TODO"
          (Nothing, Nothing, Nothing) -> return v
          (Nothing, Just (IntVal to), Nothing) -> return $ V.take (fromInteger to) v
          (Just (IntVal from), Nothing, Nothing) -> return $ V.drop (fromInteger from) v
          (Just (IntVal from), Just (IntVal to), Nothing) -> return $ V.drop (fromInteger from) (V.take (fromInteger to) v)
          (_, _, _) -> throwInternalError "type error"
      _ -> throwInternalError "type error"

evalCall :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Expr' -> [Expr'] -> m Value
evalCall f args = maybe id wrapAt (loc' f) $ do
  f <- evalExpr f
  args <- mapM evalExpr args
  evalCall' f args

evalCall' :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Value -> [Value] -> m Value
evalCall' f actualArgs = case f of
  BuiltinVal b -> do
    evalBuiltin b actualArgs
  ClosureVal local formalArgs body -> do
    when (length formalArgs /= length actualArgs) $ do
      throwInternalError "wrong number of arguments"
    savedLocal <- get
    put local
    mapM_ (uncurry assign) (zip (map fst formalArgs) actualArgs)
    v <- evalStatements body
    put savedLocal
    case v of
      Just v -> return v
      Nothing -> throwRuntimeError "it reaches the end of function without return"
  _ -> throwRuntimeError "type error"

-- | `evalStatement` evaluates statements of our restricted Python-like language.
-- When a statement is evaluated, it returns a value \(v\), doesn't return anything \(\mathbf{stop}\), or fails \(\mathbf{err}\).
-- Also it updates the environment function \(\mu\) from variables to values.
--
-- === Rules for \(\mathbf{return}~ e\)
--
-- \[
--     \cfrac{
--         e \mid \mu \Downarrow v
--     }{
--         \mathbf{return}~ e \mid \mu \Downarrow v \mid \mu
--     }
-- \]
--
-- === Rules for \(y \operatorname{binop} = e\)
--
-- \[
--     \cfrac{
--         y \operatorname{binop} e \mid \mu \Downarrow v
--     }{
--         y \operatorname{binop} = e \mid \mu \Downarrow \mathbf{stop} \mid (y \mapsto v; \mu)
--     }
-- \]
--
-- === Rules for \(y := e\)
--
-- \[
--     \cfrac{
--         e \mid \mu \Downarrow v
--      }{
--          y \operatorname{binop} = e \mid \mu \Downarrow \mathbf{stop} \mid (y \mapsto v; \mu)
--      }
-- \]
--
-- === Rules for \(\mathbf{for}~ y ~\mathbf{in}~ e \colon\quad \mathrm{stmt}; \mathrm{stmt}; \dots; \mathrm{stmt}\)
--
-- \[
--     \cfrac{
--         e \mid \mu \Downarrow \mathbf{nil}
--     }{
--         (\mathbf{for}~ y ~\mathbf{in}~ e \colon~ \vec{\mathrm{stmt}}) \mid \mu \Downarrow \mathbf{stop} \mid \mu
--     }
-- \]
--
-- \[
--     \cfrac{
--         e \mid \mu \Downarrow \mathbf{cons}(v_1, v_2)
--         \qquad \vec{\mathrm{stmt}} \mid (y \mapsto v_1; \mu) \Downarrow v \mid \mu'
--     }{
--         (\mathbf{for}~ y ~\mathbf{in}~ e \colon~ \vec{\mathrm{stmt}}) \mid \mu \Downarrow v \mid \mu'
--     }
-- \]
--
-- \[
--     \cfrac{
--         e \mid \mu \Downarrow \mathbf{cons}(v_1, v_2)
--         \qquad \vec{\mathrm{stmt}} \mid (y \mapsto v_1; \mu) \Downarrow \mathbf{stop} \mid \mu'
--         \qquad (\mathbf{for}~ y ~\mathbf{in}~ v_2 \colon~ \vec{\mathrm{stmt}}) \mid \mu' \Downarrow a \mid \mu''
--     }{
--         (\mathbf{for}~ y ~\mathbf{in}~ e \colon~ \vec{\mathrm{stmt}}) \mid \mu \Downarrow a \mid \mu''
--     }
--     \qquad{(a \in \lbrace v, \mathbf{stop} \rbrace)}
-- \]
--
-- It assumes the program is properly alpha-converted, i.e. `doesntHaveLeakOfLoopCounters`. So it leaks loop counters to out of loops.
--
-- === Rules for \(\mathbf{if}~ e \colon\quad \mathrm{stmt}; \mathrm{stmt}; \dots; \mathrm{stmt};\quad \mathbf{else}\colon\quad \mathrm{stmt}; \mathrm{stmt}; \dots; \mathrm{stmt}\)
--
-- \[
--     \cfrac{
--         e \mid \mu \Downarrow \mathbf{true}
--         \qquad \vec{\mathrm{stmt}} _ 1 \mid \mu \Downarrow a \mid \mu'
--     }{
--         (\mathbf{if}~ e \colon~ \vec{\mathrm{stmt}} _ 1 ~\mathbf{else}\colon~ \vec{\mathrm{stmt}} _ 2) \mid \mu \Downarrow a \mid \mu'
--     }
--     \qquad{(a \in \lbrace v, \mathbf{stop} \rbrace)}
-- \]
--
-- \[
--     \cfrac{
--         e \mid \mu \Downarrow \mathbf{false}
--         \qquad \vec{\mathrm{stmt}} _ 2 \mid \mu \Downarrow a \mid \mu'
--     }{
--         (\mathbf{if}~ e \colon~ \vec{\mathrm{stmt}} _ 1 ~\mathbf{else}\colon~ \vec{\mathrm{stmt}} _ 2) \mid \mu \Downarrow a \mid \mu'
--     }
--     \qquad{(a \in \lbrace v, \mathbf{stop} \rbrace)}
-- \]
--
-- === Rules for \(\mathbf{assert}~ e\)
--
-- \[
--     \cfrac{
--         e \mid \mu \Downarrow \mathbf{true}
--     }{
--         \mathbf{assert}~ e \mid \mu \Downarrow \mathbf{stop}
--     }
-- \]
--
-- \[
--     \cfrac{
--         e \mid \mu \Downarrow \mathbf{false}
--     }{
--         \mathbf{assert}~ e \mid \mu \Downarrow \mathbf{err}
--     }
-- \]
evalStatement :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Statement -> m (Maybe Value)
evalStatement = \case
  Return e -> do
    v <- evalExpr e
    return $ Just v
  AugAssign x op e -> do
    v1 <- evalTarget x
    v2 <- evalExpr e
    v <- evalBinOp v1 op v2
    assignTarget x v
    return Nothing
  AnnAssign x _ e -> do
    v <- evalExpr e
    assignTarget x v
    return Nothing
  For x iter body -> do
    iter <- evalExpr iter
    case iter of
      ListVal iter -> do
        let go [] = return Nothing
            go (it : iter) = do
              assignTarget x it
              v <- evalStatements body
              case v of
                Just v -> return $ Just v
                Nothing -> go iter
        go (V.toList iter)
      _ -> maybe id wrapAt (loc' x) $ do
        throwInternalError "type error"
  If pred body1 body2 -> do
    pred <- evalExpr pred
    if pred /= BoolVal False
      then evalStatements body1
      else evalStatements body2
  Assert e -> maybe id wrapAt (loc' e) $ do
    v <- evalExpr e
    when (v == BoolVal False) $ do
      throwRuntimeError "assertion failure"
    return Nothing

-- | `evalStatements` evaluates sequences of statements of our restricted Python-like language.
--
-- \[
--     \cfrac{\mathrm{stmt} _ 0 \mid \mu \Downarrow v \mid \mu'}{\mathrm{stmt} _ 0; \mathrm{stmt} _ 1; \dots; \mathrm{stmt} _ {n-1} \mid \mu \Downarrow v \mid \mu'}
-- \]
--
-- \[
--     \cfrac{\mathrm{stmt} _ 0 \mid \mu \Downarrow \mathbf{stop} \mid \mu' \qquad \mathrm{stmt} _ 1; \dots; \mathrm{stmt} _ {n-1} \mid \mu' \Downarrow a \mid \mu''}{\mathrm{stmt} _ 0; \mathrm{stmt} _ 1; \dots; \mathrm{stmt} _ {n-1} \mid \mu \Downarrow a \mid \mu''}
--     \qquad{(a \in \lbrace v, \mathbf{stop} \rbrace)}
-- \]
--
-- \[
--     \epsilon \mid \mu \Downarrow \mathbf{stop} \mid \mu
-- \]
evalStatements :: (MonadReader Global m, MonadState Local m, MonadError Error m) => [Statement] -> m (Maybe Value)
evalStatements [] = return Nothing
evalStatements (stmt : stmts) = do
  v <- evalStatement stmt
  case v of
    Just v -> return $ Just v
    Nothing -> evalStatements stmts

execToplevelStatement :: (MonadState Global m, MonadError Error m) => ToplevelStatement -> m ()
execToplevelStatement = \case
  ToplevelAnnAssign x _ e -> do
    global <- get
    v <- runWithGlobal global e
    put $ Global (M.insert (value' x) v (unGlobal global))
  ToplevelFunctionDef f args _ body -> do
    global <- get
    let v = ClosureVal (Local M.empty) (map (first value') args) body
    put $ Global (M.insert (value' f) v (unGlobal global))
  ToplevelAssert e -> do
    global <- get
    v <- runWithGlobal global e
    when (v /= BoolVal True) $ do
      throwRuntimeError "assertion failure"

runWithGlobal :: MonadError Error m => Global -> Expr' -> m Value
runWithGlobal global e = wrapError' "Jikka.RestrictedPython.Evaluate" $ do
  runReaderT (evalStateT (evalExpr e) (Local M.empty)) global

-- | `makeGlobal` packs toplevel definitions into `Global`.
-- This assumes `doesntHaveLeakOfLoopCounters`.
makeGlobal :: MonadError Error m => Program -> m Global
makeGlobal prog = wrapError' "Jikka.RestrictedPython.Evaluate" $ do
  ensureDoesntHaveLeakOfLoopCounters prog
  execStateT (mapM_ execToplevelStatement prog) initialGlobal

run :: MonadError Error m => Program -> Expr' -> m Value
run prog e = do
  global <- makeGlobal prog
  runWithGlobal global e

evalBinOp :: MonadError Error m => Value -> Operator -> Value -> m Value
evalBinOp v1 op v2 = wrapError' ("calculating " ++ formatOperator op ++ " operator") $ do
  v1 <- toInt v1
  v2 <- toInt v2
  v <- case (op, v2) of
    (Add, _) -> return $ v1 + v2
    (Sub, _) -> return $ v1 - v2
    (Mult, _) -> return $ v1 * v2
    (MatMult, _) -> throwInternalError "matmul operator ('@') is not supported"
    (Div, _) -> throwInternalError "floatdiv operator ('/') is not supported"
    (FloorDiv, 0) -> throwRuntimeError "division by zero"
    (FloorDiv, _) -> return $ v1 `div` v2
    (FloorMod, 0) -> throwRuntimeError "division by zero"
    (FloorMod, _) -> return $ v1 `mod` v2
    (CeilDiv, 0) -> throwRuntimeError "division by zero"
    (CeilDiv, _) -> return $ (v1 + v2 - 1) `div` v2
    (CeilMod, 0) -> throwRuntimeError "division by zero"
    (CeilMod, _) -> return $ (v1 + v2 - 1) `mod` v2
    (Pow, _) -> return $ v1 ^ v2
    (BitLShift, _) -> return $ shiftL v1 (fromInteger v2)
    (BitRShift, _) -> return $ shiftR v1 (fromInteger v2)
    (BitOr, _) -> return $ v1 .|. v2
    (BitXor, _) -> return $ v1 `xor` v2
    (BitAnd, _) -> return $ v1 .&. v2
    (Max, _) -> return $ max v1 v2
    (Min, _) -> return $ min v1 v2
  return $ IntVal v

evalBuiltin :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Builtin -> [Value] -> m Value
evalBuiltin b args = wrapError' ("calling " ++ formatBuiltin b) $ do
  let go1' t1 ret f = case args of
        [v1] -> ret <$> (f =<< t1 v1)
        _ -> throwInternalError $ "expected 1 argument, got " ++ show (length args)
  let go1 t1 ret f = go1' t1 ret (return . f)
  let go2' t1 t2 ret f = case args of
        [v1, v2] -> ret <$> join (f <$> t1 v1 <*> t2 v2)
        _ -> throwInternalError $ "expected 2 arguments, got " ++ show (length args)
  let go2 t1 t2 ret f = go2' t1 t2 ret ((return .) . f)
  let go3 t1 t2 t3 ret f = case args of
        [v1, v2, v3] -> ret <$> (f <$> t1 v1 <*> t2 v2 <*> t3 v3)
        _ -> throwInternalError $ "expected 3 arguments, got " ++ show (length args)
  let goN' t ret f = ret <$> (f =<< mapM t args)
  let goN t ret f = goN' t ret (return . f)
  let zipN acc [] = reverse acc
      zipN acc xss | any null xss = reverse acc
      zipN acc xss = zipN (map head xss : acc) (map tail xss)
  case b of
    BuiltinAbs -> go1 toInt IntVal abs
    BuiltinPow -> go2 toInt toInt IntVal (^)
    BuiltinModPow -> go3 toInt toInt toInt IntVal $ \x k m -> (x ^ k) `mod` m
    BuiltinAll -> go1 toBoolList BoolVal minimum
    BuiltinAny -> go1 toBoolList BoolVal maximum
    BuiltinDivMod -> go2' toInt toInt TupleVal $ \a b -> case b of
      0 -> throwRuntimeError "division by zero"
      _ -> return [IntVal (a `div` b), IntVal (a `mod` b)]
    BuiltinSorted _ -> go1 toList ListVal (V.fromList . sortBy compareValues' . V.toList)
    BuiltinEnumerate _ -> go1 toList ListVal (V.fromList . zipWith (\i x -> TupleVal [IntVal i, x]) [0 ..] . V.toList)
    BuiltinBool _ -> go1' pure BoolVal $ \case
      IntVal n -> return $ n /= 0
      BoolVal p -> return p
      ListVal xs -> return $ not (V.null xs)
      TupleVal xs -> return $ not (null xs)
      _ -> throwInternalError "type error"
    BuiltinInt _ -> go1' return IntVal $ \case
      IntVal n -> return n
      BoolVal p -> return $ if p then 1 else 0
      _ -> throwInternalError "type error"
    BuiltinTuple _ -> goN pure TupleVal id
    BuiltinSum -> go1 toIntList IntVal sum
    BuiltinZip _ -> goN toList ListVal (V.fromList . map TupleVal . zipN [] . map V.toList)
    BuiltinFilter _ -> go2' pure toList ListVal $ \f xs -> do
      let go x = do
            pred <- evalCall' f [x]
            case pred of
              BoolVal True -> return $ Just x
              BoolVal False -> return Nothing
              _ -> throwInternalError "type error"
      V.mapMaybeM go xs
    BuiltinLen _ -> go1 toList IntVal (fromIntegral . V.length)
    BuiltinList _ -> go1 toList ListVal id
    BuiltinRange1 -> go1 toInt ListVal $ \to -> V.fromList (map IntVal [0 .. to - 1])
    BuiltinRange2 -> go2 toInt toInt ListVal $ \from to -> V.fromList (map IntVal [from .. to - 1])
    BuiltinRange3 -> go3 toInt toInt toInt ListVal $ \from to step -> V.fromList (map IntVal [from, from + step .. to - 1])
    BuiltinMap _ _ -> goN' pure ListVal $ \case
      [] -> throwInternalError "type error"
      f : args -> do
        args <- mapM toList args
        V.fromList <$> mapM (evalCall' f) (zipN [] (map V.toList args))
    BuiltinReversed _ -> go1 toList ListVal V.reverse
    BuiltinMin1 _ -> go1 toList id (minimumBy compareValues')
    BuiltinMin _ _ -> goN pure id (minimumBy compareValues')
    BuiltinMax1 _ -> go1 toList id (maximumBy compareValues')
    BuiltinMax _ _ -> goN pure id (maximumBy compareValues')
    BuiltinArgMax _ -> go1 toList IntVal $ \xs -> snd (maximumBy (\(x, i) (y, j) -> compareValues' x y <> compare i j) (zip (V.toList xs) [0 ..]))
    BuiltinArgMin _ -> go1 toList IntVal $ \xs -> snd (minimumBy (\(x, i) (y, j) -> compareValues' x y <> compare i j) (zip (V.toList xs) [0 ..]))
    BuiltinCeilDiv -> go2' toInt toInt IntVal $ \a b -> if b == 0 then throwRuntimeError "division by zero" else return $ (a + b - 1) `div` b
    BuiltinCeilMod -> go2' toInt toInt IntVal $ \a b -> if b == 0 then throwRuntimeError "division by zero" else return $ (a + b - 1) `mod` b
    BuiltinFloorDiv -> go2' toInt toInt IntVal $ \a b -> if b == 0 then throwRuntimeError "division by zero" else return $ a `div` b
    BuiltinFloorMod -> go2' toInt toInt IntVal $ \a b -> if b == 0 then throwRuntimeError "division by zero" else return $ a `mod` b
    BuiltinGcd -> go2 toInt toInt IntVal gcd
    BuiltinLcm -> go2 toInt toInt IntVal lcm
    BuiltinModInv -> go2' toInt toInt IntVal $ \_ _ -> throwInternalError "Jikka.RestrictedPython.Evaluate.evalBuiltin: TODO"
    BuiltinProduct -> go1 toIntList IntVal product
    BuiltinFact -> go1' toInt IntVal $ \n -> if 0 <= n then return $ fact n else throwRuntimeError "invalid argument"
    BuiltinChoose -> go2' toInt toInt IntVal $ \n r -> if 0 <= r && r <= n then return $ choose n r else throwRuntimeError "invalid argument"
    BuiltinPermute -> go2' toInt toInt IntVal $ \n r -> if 0 <= r && r <= n then return $ permute n r else throwRuntimeError "invalid argument"
    BuiltinMultiChoose -> go2' toInt toInt IntVal $ \n r -> if 0 <= r && r <= n then return $ multichoose n r else throwRuntimeError "invalid argument"
