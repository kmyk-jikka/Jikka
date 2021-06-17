{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.RestrictedPython.Evaluate
-- Description : evaluates programs of the restricted Python.
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Evaluate
  ( run,
    Value (..),
    evalExpr,
    evalStatement,
    evalStatements,
    execToplevelStatement,
  )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Expr

-- | `Value` is the values of our restricted Python-like language.
--
-- \[
--     \begin{array}{rl}
--         v ::= & \dots, -2, -1, 0, 1, 2, \dots \\
--         \vert & \mathbf{false}, \mathbf{true} \\
--         \vert & \mathbf{nil} \\
--         \vert & \mathbf{cons}(v, v) \\
--         \vert & (v, v, \dots, v) \\
--         \vert & \lambda _ \mu x x \dots x. e \\
--     \end{array}
-- \]
data Value
  = IntVal Integer
  | BoolVal Bool
  | ListVal (V.Vector Value)
  | TupleVal [Value]
  | ClosureVal Local [Ident] [Statement]
  deriving (Eq, Ord, Show, Read)

newtype Global = Global
  { unGlobal :: M.Map Ident Value
  }
  deriving (Eq, Ord, Show, Read)

newtype Local = Local
  { unLocal :: M.Map Ident Value
  }
  deriving (Eq, Ord, Show, Read)

assign :: MonadState Local m => Ident -> Value -> m ()
assign x v = modify' (Local . M.insert x v . unLocal)

lookupLocal :: (MonadState Local m, MonadError Error m) => Ident -> m Value
lookupLocal x = do
  local <- get
  case M.lookup x (unLocal local) of
    Just v -> return v
    Nothing -> throwRuntimeError "undefined variable"

assignSubscriptedTarget :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Target -> Expr -> Value -> m ()
assignSubscriptedTarget f index v = do
  let go f indices = case f of
        SubscriptTrg f index -> go f (index : indices)
        NameTrg x -> return (x, indices)
        TupleTrg _ -> throwRuntimeError "cannot subscript a tuple target"
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
        (_, _) -> throwRuntimeError "type error"
  f <- go f indices
  assign x f

assignTarget :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Target -> Value -> m ()
assignTarget trg v = do
  case trg of
    SubscriptTrg f index -> do
      assignSubscriptedTarget f index v
    NameTrg x -> do
      assign x v
    TupleTrg xs -> do
      case v of
        TupleVal vs -> do
          when (length xs /= length vs) $ do
            throwRuntimeError "the lengths of tuple are different"
          forM_ (zip xs vs) $ \(x, v) -> do
            assignTarget x v
        _ -> throwRuntimeError "cannot unpack non-tuple value"

evalTarget :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Target -> m Value
evalTarget = \case
  SubscriptTrg f index -> do
    f <- evalTarget f
    index <- evalExpr index
    case (f, index) of
      (ListVal f, IntVal index) -> do
        when (index < 0 || fromIntegral (V.length f) <= index) $ do
          throwRuntimeError "list index out of range"
        return $ f V.! fromInteger index
      (_, _) -> throwRuntimeError "type error"
  NameTrg x -> lookupLocal x
  TupleTrg xs -> TupleVal <$> mapM evalTarget xs

evalBinOp :: MonadError Error m => Value -> Operator -> Value -> m Value
evalBinOp v1 op v2 = do
  (v1, v2) <- case (v1, v2) of
    (IntVal v1, IntVal v2) -> return (v1, v2)
    (_, _) -> throwRuntimeError "type error"
  v <- case (op, v2) of
    (Add, _) -> return $ v1 + v2
    (Sub, _) -> return $ v1 - v2
    (Mult, _) -> return $ v1 * v2
    (MatMult, _) -> throwRuntimeError "matmul operator ('@') is not supported"
    (Div, _) -> throwRuntimeError "floatdiv operator ('/') is not supported"
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
evalExpr :: (MonadReader Global m, MonadState Local m, MonadError Error m) => Expr -> m Value
evalExpr = \case
  BoolOp e1 op e2 -> do
    v1 <- evalExpr e1
    case (v1, op) of
      (BoolVal False, And) -> return $ BoolVal False
      (BoolVal True, And) -> evalExpr e2
      (BoolVal False, Or) -> evalExpr e2
      (BoolVal True, Or) -> return $ BoolVal True
      (BoolVal False, Implies) -> return $ BoolVal True
      (BoolVal True, Implies) -> evalExpr e2
      (_, _) -> throwRuntimeError "type error"
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
      (_, _) -> throwRuntimeError "type error"
  Lambda args body -> do
    savedLocal <- get
    return $ ClosureVal savedLocal (map fst args) [Return body]
  IfExp e1 e2 e3 -> do
    v1 <- evalExpr e1
    case v1 of
      BoolVal True -> evalExpr e2
      BoolVal False -> evalExpr e3
      _ -> throwRuntimeError "type error"
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
      _ -> throwRuntimeError "type error"
  Compare e1 _ e2 -> do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    return $ BoolVal (v1 == v2)
  Call f actualArgs -> do
    f <- evalExpr f
    actualArgs <- mapM evalExpr actualArgs
    case f of
      ClosureVal local formalArgs body -> do
        when (length formalArgs /= length actualArgs) $ do
          throwRuntimeError "wrong number of arguments"
        savedLocal <- get
        put local
        mapM_ (uncurry assign) (zip formalArgs actualArgs)
        v <- evalStatements body
        put savedLocal
        case v of
          Just v -> return v
          Nothing -> throwRuntimeError "it reaches the end of function without return"
      _ -> throwRuntimeError "type error"
  Constant const ->
    return $ case const of
      ConstNone -> TupleVal []
      ConstInt v -> IntVal v
      ConstBool v -> BoolVal v
  Subscript e1 e2 -> do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
      (ListVal v1, IntVal v2) -> do
        when (v2 < 0 || fromIntegral (V.length v1) <= v2) $ do
          throwRuntimeError "list index out of range"
        return $ v1 V.! fromInteger v2
      _ -> throwRuntimeError "type error"
  Name x -> do
    local <- get
    case M.lookup x (unLocal local) of
      Just v -> return v
      Nothing -> do
        global <- ask
        case M.lookup x (unGlobal global) of
          Just v -> return v
          Nothing -> throwRuntimeError $ "undefined variable: " ++ show x
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
          (_, _, Just _) -> throwRuntimeError "slice with step is TODO"
          (Nothing, Nothing, Nothing) -> return v
          (Nothing, Just (IntVal to), Nothing) -> return $ V.take (fromInteger to) v
          (Just (IntVal from), Nothing, Nothing) -> return $ V.drop (fromInteger from) v
          (Just (IntVal from), Just (IntVal to), Nothing) -> return $ V.drop (fromInteger from) (V.take (fromInteger to) v)
          (_, _, _) -> throwRuntimeError "type error"
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
-- It assumes the program is properly alpha-converted and leaks the scope of loop counters.
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
      _ -> throwRuntimeError "type error"
  If pred body1 body2 -> do
    pred <- evalExpr pred
    if pred /= BoolVal False
      then evalStatements body1
      else evalStatements body2
  Assert e -> do
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
    v <- run' global e
    put $ Global (M.insert x v (unGlobal global))
  ToplevelFunctionDef f args _ body -> do
    global <- get
    let v = ClosureVal (Local M.empty) (map fst args) body
    put $ Global (M.insert f v (unGlobal global))
  ToplevelAssert e -> do
    global <- get
    v <- run' global e
    when (v /= BoolVal True) $ do
      throwRuntimeError "assertion failure"

run' :: MonadError Error m => Global -> Expr -> m Value
run' global e = runReaderT (evalStateT (evalExpr e) (Local M.empty)) global

run :: MonadError Error m => Program -> Expr -> m Value
run prog e = do
  global <- execStateT (mapM_ execToplevelStatement prog) (Global M.empty)
  run' global e
