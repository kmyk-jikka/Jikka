{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Jikka.Core.Evaluate
-- Description : exexutes the expr of our core language.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Core.Evaluate` evaluates exprs to values. Also this recognizes users' inputs at once.
--
-- The implementation assumes that all variable names don't conflict even when their scopes are distinct.
-- Also this assumes that exprs allow the eager evaluation. Please use `Jikka.Core.Convert.MakeEager` if needed.
module Jikka.Core.Evaluate
  ( run,
    run',
    Value (..),
    Token (..),
    tokenize,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Bits
import Data.List (sort)
import qualified Data.Vector as V
import Jikka.Common.Error
import Jikka.Core.Convert.TypeInfer (builtinToType)
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Value
import Text.Read (readEither)

-- -----------------------------------------------------------------------------
-- inputs

newtype Token = Token {unToken :: String}
  deriving (Eq, Ord, Show, Read)

tokenize :: String -> [Token]
tokenize = map Token . words

readToken :: (MonadError Error m, Read a) => Token -> m a
readToken token = case readEither (unToken token) of
  Left msg -> throwWrongInputError msg
  Right a -> return a

readInputList :: MonadError Error m => Type -> Int -> [Token] -> m ([Value], [Token])
readInputList t = go
  where
    go n _ | n < 0 = throwWrongInputError "negative size of list in input"
    go 0 tokens = return ([], tokens)
    go n tokens = do
      (x, tokens) <- readInput t tokens
      (xs, tokens) <- go (n - 1) tokens
      return (x : xs, tokens)

readInputMap :: MonadError Error m => [Type] -> [Token] -> m ([Value], [Token])
readInputMap ts tokens = case ts of
  [] -> return ([], tokens)
  (t : ts) -> do
    (val, tokens) <- readInput t tokens
    (vals, tokens) <- readInputMap ts tokens
    return (val : vals, tokens)

readInput :: MonadError Error m => Type -> [Token] -> m (Value, [Token])
readInput t tokens = case (t, tokens) of
  (VarTy a, _) -> throwInternalError $ "input type is undetermined: " ++ show a
  (IntTy, token : tokens) -> do
    n <- readToken token
    return (ValInt n, tokens)
  (BoolTy, token : tokens) -> do
    p <- readToken token
    return (ValBool p, tokens)
  (ListTy t, token : tokens) -> do
    n <- readToken token
    (a, tokens) <- readInputList t n tokens
    return (ValList (V.fromList a), tokens)
  (TupleTy ts, _) -> do
    let readInput' :: MonadError Error m => Type -> StateT [Token] m Value
        readInput' t = StateT (readInput t)
    (values, tokens) <- runStateT (mapM readInput' ts) tokens
    return (ValTuple values, tokens)
  (FunTy _ _, _) -> throwWrongInputError "we cannot use functions as inputs"
  (_, []) -> throwWrongInputError "it reaches EOF"

-- -----------------------------------------------------------------------------
-- builtins

floorDiv :: MonadError Error m => Integer -> Integer -> m Integer
floorDiv _ 0 = throwRuntimeError "zero div"
floorDiv a b = return (a `div` b)

floorMod :: MonadError Error m => Integer -> Integer -> m Integer
floorMod _ 0 = throwRuntimeError "zero div"
floorMod a b = return (a `mod` b)

ceilDiv :: MonadError Error m => Integer -> Integer -> m Integer
ceilDiv _ 0 = throwRuntimeError "zero div"
ceilDiv a b = return ((a + b - 1) `div` b)

ceilMod :: MonadError Error m => Integer -> Integer -> m Integer
ceilMod _ 0 = throwRuntimeError "zero div"
ceilMod a b = return (a - ((a + b - 1) `div` b) * b)

natind :: MonadError Error m => Value -> Value -> Integer -> m Value
natind _ _ n | n < 0 = throwRuntimeError $ "negative number for mathematical induction: " ++ show n
natind base _ 0 = return base
natind base step n = do
  val <- natind base step (n - 1)
  callValue step [val, ValInt (n - 1)]

minimumEither :: (MonadError Error m, Ord a) => [a] -> m a
minimumEither [] = throwRuntimeError "there is no minimum for the empty list"
minimumEither a = return $ minimum a

maximumEither :: (MonadError Error m, Ord a) => [a] -> m a
maximumEither [] = throwRuntimeError "there is no maximum for the empty list"
maximumEither a = return $ maximum a

argminEither :: (MonadError Error m, Ord a) => [a] -> m Integer
argminEither [] = throwRuntimeError "there is no minimum for the empty list"
argminEither a = return $ snd (minimum (zip a [0 ..]))

argmaxEither :: (MonadError Error m, Ord a) => [a] -> m Integer
argmaxEither [] = throwRuntimeError "there is no maximum for the empty list"
argmaxEither a = return $ snd (maximum (zip a [0 ..]))

inv :: MonadError Error m => Integer -> Integer -> m Integer
inv a m | m <= 0 || a `mod` m == 0 = throwRuntimeError $ "invalid argument for inv: " ++ show (a, m)
inv _ _ = throwInternalError "TODO: implement inv()"

powmod :: MonadError Error m => Integer -> Integer -> Integer -> m Integer
powmod _ _ m | m <= 0 = throwRuntimeError $ "invalid argument for powmod: MOD = " ++ show m
powmod a b m = return $ (a ^ b) `mod` m

tabulate :: MonadError Error m => Integer -> Value -> m (V.Vector Value)
tabulate n f = V.fromList <$> mapM (\i -> callValue f [ValInt i]) [0 .. n - 1]

map' :: MonadError Error m => Value -> V.Vector Value -> m (V.Vector Value)
map' f a = V.fromList <$> mapM (\val -> callValue f [val]) (V.toList a)

atEither :: MonadError Error m => V.Vector a -> Integer -> m a
atEither xs i = case xs V.!? fromInteger i of
  Just x -> return x
  Nothing -> throwRuntimeError $ "out of bounds: " ++ show (V.length xs, i)

sortVector :: Ord a => V.Vector a -> V.Vector a
sortVector = V.fromList . sort . V.toList

range1 :: MonadError Error m => Integer -> m (V.Vector Value)
range1 n | n < 0 = throwRuntimeError $ "invalid argument for range1: " ++ show n
range1 n = return $ V.fromList (map ValInt [0 .. n - 1])

range2 :: MonadError Error m => Integer -> Integer -> m (V.Vector Value)
range2 l r | l > r = throwRuntimeError $ "invalid argument for range2: " ++ show (l, r)
range2 l r = return $ V.fromList (map ValInt [l .. r - 1])

range3 :: MonadError Error m => Integer -> Integer -> Integer -> m (V.Vector Value)
range3 l r step | not (l <= r && step >= 0) = throwRuntimeError $ "invalid argument for range3: " ++ show (l, r, step)
range3 l r step = return $ V.fromList (map ValInt [l, l + step .. r])

fact :: MonadError Error m => Integer -> m Integer
fact n | n < 0 = throwRuntimeError $ "invalid argument for fact: " ++ show n
fact n = return $ product [1 .. n]

choose :: MonadError Error m => Integer -> Integer -> m Integer
choose n r | not (0 <= r && r <= n) = throwRuntimeError $ "invalid argument for choose: " ++ show (n, r)
choose n r = return $ product [n - r + 1 .. n] `div` product [1 .. r]

permute :: MonadError Error m => Integer -> Integer -> m Integer
permute n r | not (0 <= r && r <= n) = throwRuntimeError $ "invalid argument for choose: " ++ show (n, r)
permute n r = return $ product [n - r + 1 .. n]

multichoose :: MonadError Error m => Integer -> Integer -> m Integer
multichoose n r | not (0 <= r && r <= n) = throwRuntimeError $ "invalid argument for multichoose: " ++ show (n, r)
multichoose 0 0 = return 1
multichoose n r = choose (n + r - 1) r

-- -----------------------------------------------------------------------------
-- evaluator

callBuiltin :: MonadError Error m => Builtin -> [Value] -> m Value
callBuiltin builtin args = case (builtin, args) of
  -- arithmetical functions
  (Negate, [ValInt n]) -> return $ ValInt (- n)
  (Plus, [ValInt a, ValInt b]) -> return $ ValInt (a + b)
  (Minus, [ValInt a, ValInt b]) -> return $ ValInt (a - b)
  (Mult, [ValInt a, ValInt b]) -> return $ ValInt (a * b)
  (FloorDiv, [ValInt a, ValInt b]) -> ValInt <$> floorDiv a b
  (FloorMod, [ValInt a, ValInt b]) -> ValInt <$> floorMod a b
  (CeilDiv, [ValInt a, ValInt b]) -> ValInt <$> ceilDiv a b
  (CeilMod, [ValInt a, ValInt b]) -> ValInt <$> ceilMod a b
  (Pow, [ValInt a, ValInt b]) -> return $ ValInt (a ^ b)
  -- induction functions
  (NatInd _, [base, step, ValInt n]) -> natind base step n
  -- advanced arithmetical functions
  (Abs, [ValInt n]) -> return $ ValInt (abs n)
  (Gcd, [ValInt a, ValInt b]) -> return $ ValInt (gcd a b)
  (Lcm, [ValInt a, ValInt b]) -> return $ ValInt (lcm a b)
  (Min2 IntTy, [ValInt a, ValInt b]) -> return $ ValInt (min a b) -- TODO: allow non-integers
  (Max2 IntTy, [ValInt a, ValInt b]) -> return $ ValInt (max a b) -- TODO: allow non-integers
  -- logical functions
  (Not, [ValBool p]) -> return $ ValBool (not p)
  (And, [ValBool p, ValBool q]) -> return $ ValBool (p && q)
  (Or, [ValBool p, ValBool q]) -> return $ ValBool (p || q)
  (Implies, [ValBool p, ValBool q]) -> return $ ValBool (not p || q)
  (If _, [ValBool p, a, b]) -> return $ if p then a else b
  -- bitwise functions
  (BitNot, [ValInt a]) -> return $ ValInt (complement a)
  (BitAnd, [ValInt a, ValInt b]) -> return $ ValInt (a .&. b)
  (BitOr, [ValInt a, ValInt b]) -> return $ ValInt (a .|. b)
  (BitXor, [ValInt a, ValInt b]) -> return $ ValInt (a `xor` b)
  (BitLeftShift, [ValInt a, ValInt b]) -> return $ ValInt (a `shift` fromInteger b)
  (BitRightShift, [ValInt a, ValInt b]) -> return $ ValInt (a `shift` fromInteger (- b))
  -- modular functions
  (ModInv, [ValInt a, ValInt b]) -> ValInt <$> inv a b
  (ModPow, [ValInt a, ValInt b, ValInt c]) -> ValInt <$> powmod a b c
  -- list functions
  (Cons _, [x, ValList xs]) -> return $ ValList (V.cons x xs)
  (Len _, [ValList a]) -> return $ ValInt (fromIntegral (V.length a))
  (Tabulate _, [ValInt n, f]) -> ValList <$> tabulate n f
  (Map _ _, [f, ValList a]) -> ValList <$> map' f a
  (At _, [ValList a, ValInt n]) -> atEither a n
  (Sum, [ValList a]) -> ValInt . sum <$> valueToIntList a
  (Product, [ValList a]) -> ValInt . product <$> valueToIntList a
  (Min1 IntTy, [ValList a]) -> ValInt <$> (minimumEither =<< valueToIntList a) -- TODO: allow non-integers
  (Max1 IntTy, [ValList a]) -> ValInt <$> (maximumEither =<< valueToIntList a) -- TODO: allow non-integers
  (ArgMin IntTy, [ValList a]) -> ValInt <$> (argminEither =<< valueToIntList a) -- TODO: allow non-integers
  (ArgMax IntTy, [ValList a]) -> ValInt <$> (argmaxEither =<< valueToIntList a) -- TODO: allow non-integers
  (All, [ValList a]) -> ValBool . and <$> valueToBoolList a
  (Any, [ValList a]) -> ValBool . or <$> valueToBoolList a
  (Sorted _, [ValList a]) -> return $ ValList (sortVector a)
  (List _, [ValList a]) -> return $ ValList a
  (Reversed _, [ValList a]) -> return $ ValList (V.reverse a)
  (Range1, [ValInt n]) -> ValList <$> range1 n
  (Range2, [ValInt l, ValInt r]) -> ValList <$> range2 l r
  (Range3, [ValInt l, ValInt r, ValInt step]) -> ValList <$> range3 l r step
  -- tuple functions
  (Tuple _, xs) -> return $ ValTuple xs
  (Proj _ n, [ValTuple xs]) -> return $ xs !! n
  -- comparison
  (LessThan IntTy, [ValInt a, ValInt b]) -> return $ ValBool (a < b) -- TODO: allow non-integers
  (LessEqual IntTy, [ValInt a, ValInt b]) -> return $ ValBool (a <= b) -- TODO: allow non-integers
  (GreaterThan IntTy, [ValInt a, ValInt b]) -> return $ ValBool (a > b) -- TODO: allow non-integers
  (GreaterEqual IntTy, [ValInt a, ValInt b]) -> return $ ValBool (a >= b) -- TODO: allow non-integers
  (Equal _, [a, b]) -> return $ ValBool (a == b)
  (NotEqual _, [a, b]) -> return $ ValBool (a /= b)
  -- combinational functions
  (Fact, [ValInt n]) -> ValInt <$> fact n
  (Choose, [ValInt n, ValInt r]) -> ValInt <$> choose n r
  (Permute, [ValInt n, ValInt r]) -> ValInt <$> permute n r
  (MultiChoose, [ValInt n, ValInt r]) -> ValInt <$> multichoose n r
  _ -> throwInternalError $ "invalid builtin call: " ++ show (builtin, args)

callLambda :: MonadError Error m => Env -> [(VarName, Type)] -> Expr -> [Value] -> m Value
callLambda env formalArgs body actualArgs = case (formalArgs, actualArgs) of
  ([], []) -> evaluateExpr env body
  ((x, _) : formalArgs, val : actualArgs) -> callLambda ((x, val) : env) formalArgs body actualArgs
  _ -> throwInternalError "wrong number of arguments for lambda function"

callValue :: MonadError Error m => Value -> [Value] -> m Value
callValue f args = case f of
  ValBuiltin builtin -> callBuiltin builtin args
  ValLambda env args' body -> callLambda env args' body args
  _ -> throwInternalError $ "call non-function: " ++ show f

evaluateExpr :: MonadError Error m => Env -> Expr -> m Value
evaluateExpr env = \case
  Var x -> case lookup x env of
    Nothing -> throwRuntimeError $ "Internal Error: undefined variable: " ++ show (unVarName x)
    Just val -> return val
  Lit lit -> return $ literalToValue lit
  App f args -> do
    f <- evaluateExpr env f
    args <- mapM (evaluateExpr env) args
    callValue f args
  Lam args body -> return $ ValLambda env args body
  Let x _ e1 e2 -> do
    v1 <- evaluateExpr env e1
    evaluateExpr ((x, v1) : env) e2

callBuiltinWithTokens :: MonadError Error m => [Token] -> Builtin -> m (Value, [Token])
callBuiltinWithTokens tokens builtin = do
  case builtinToType builtin of
    FunTy ts _ -> do
      (args, tokens) <- readInputMap ts tokens
      val <- callBuiltin builtin args
      return (val, tokens)
    _ -> throwInternalError "all builtin must be functions"

callLambdaWithTokens :: MonadError Error m => [Token] -> Env -> [(VarName, Type)] -> Expr -> m (Value, [Token])
callLambdaWithTokens tokens env args body = case args of
  ((x, t) : args) -> do
    (val, tokens) <- readInput t tokens
    callLambdaWithTokens tokens ((x, val) : env) args body
  [] -> do
    val <- evaluateExpr env body
    return (val, tokens)

evaluateToplevelExpr :: (MonadFix m, MonadError Error m) => [Token] -> Env -> ToplevelExpr -> m (Value, [Token])
evaluateToplevelExpr tokens env = \case
  ToplevelLet x _ e cont -> do
    val <- evaluateExpr env e
    evaluateToplevelExpr tokens ((x, val) : env) cont
  ToplevelLetRec f args _ body cont -> do
    val <- mfix $ \val -> evaluateExpr ((f, val) : env) (Lam args body)
    evaluateToplevelExpr tokens ((f, val) : env) cont
  ResultExpr e -> do
    val <- evaluateExpr env e
    case val of
      ValBuiltin builtin -> callBuiltinWithTokens tokens builtin
      ValLambda env args body -> callLambdaWithTokens tokens env args body
      _ -> return (val, tokens)

evaluateProgram :: (MonadFix m, MonadError Error m) => [Token] -> Program -> m Value
evaluateProgram tokens prog = do
  (val, tokens) <- evaluateToplevelExpr tokens [] prog
  if null tokens
    then return val
    else throwWrongInputError $ "evaluation succeeds, but unused inputs remain: " ++ show (val, tokens)

-- -----------------------------------------------------------------------------
-- run

run' :: (MonadFix m, MonadError Error m) => [Token] -> Program -> m Value
run' tokens prog = wrapError' "Jikka.Core.Evaluate.run' failed" $ evaluateProgram tokens prog

run :: (MonadIO m, MonadFix m, MonadError Error m) => Program -> m Value
run prog = do
  contents <- liftIO getContents
  let tokens = tokenize contents
  liftEither $ run' tokens prog
