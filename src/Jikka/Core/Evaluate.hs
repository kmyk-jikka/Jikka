{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
import Data.List (intercalate, sortBy)
import qualified Data.Vector as V
import Jikka.Common.Error
import Jikka.Common.Matrix
import qualified Jikka.Core.Convert.MakeEager as MakeEager
import Jikka.Core.Format (formatBuiltinIsolated, formatExpr)
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Runtime
import Jikka.Core.Language.TypeCheck (builtinToType)
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
  (VarTy a, _) -> throwInternalError $ "input type is undetermined: type variable " ++ unTypeName a
  (IntTy, token : tokens) -> do
    n <- readToken token
    return (ValInt n, tokens)
  (BoolTy, token : tokens) -> do
    p <-
      if unToken token == "Yes"
        then return True
        else
          if unToken token == "No"
            then return False
            else throwRuntimeError $ "boolean must be \"Yes\" or \"No\": " ++ show (unToken token)
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

natind :: MonadError Error m => Value -> Value -> Integer -> m Value
natind _ _ n | n < 0 = throwRuntimeError $ "negative number for mathematical induction: " ++ show n
natind base _ 0 = return base
natind base step n = do
  val <- natind base step (n - 1)
  callValue step [val]

tabulate :: MonadError Error m => Integer -> Value -> m (V.Vector Value)
tabulate n f = V.fromList <$> mapM (\i -> callValue f [ValInt i]) [0 .. n - 1]

map' :: MonadError Error m => Value -> V.Vector Value -> m (V.Vector Value)
map' f a = V.fromList <$> mapM (\val -> callValue f [val]) (V.toList a)

scanM :: Monad m => (a -> b -> m a) -> a -> V.Vector b -> m (V.Vector a)
scanM f y xs = do
  (ys, y) <- V.foldM (\(ys, y) x -> (y : ys,) <$> f y x) ([], y) xs
  return $ V.fromList (reverse (y : ys))

atEither :: MonadError Error m => V.Vector a -> Integer -> m a
atEither xs i = case xs V.!? fromInteger i of
  Just x -> return x
  Nothing -> throwRuntimeError $ "out of bounds: length = " ++ show (V.length xs) ++ ", index = " ++ show i

setAtEither :: MonadError Error m => V.Vector a -> Integer -> a -> m (V.Vector a)
setAtEither xs i x =
  if 0 <= i && i < fromIntegral (V.length xs)
    then return $ xs V.// [(fromInteger i, x)]
    else throwRuntimeError $ "out of bounds: length = " ++ show (V.length xs) ++ ", index = " ++ show i

sortVector :: V.Vector Value -> V.Vector Value
sortVector = V.fromList . sortBy compareValues' . V.toList

range1 :: MonadError Error m => Integer -> m (V.Vector Value)
range1 n | n < 0 = throwRuntimeError $ "invalid argument for range1: " ++ show n
range1 n = return $ V.fromList (map ValInt [0 .. n - 1])

range2 :: MonadError Error m => Integer -> Integer -> m (V.Vector Value)
range2 l r | l > r = throwRuntimeError $ "invalid argument for range2: " ++ show (l, r)
range2 l r = return $ V.fromList (map ValInt [l .. r - 1])

range3 :: MonadError Error m => Integer -> Integer -> Integer -> m (V.Vector Value)
range3 l r step | not (l <= r && step >= 0) = throwRuntimeError $ "invalid argument for range3: " ++ show (l, r, step)
range3 l r step = return $ V.fromList (map ValInt [l, l + step .. r])

-- -----------------------------------------------------------------------------
-- evaluator

callBuiltin :: MonadError Error m => Builtin -> [Value] -> m Value
callBuiltin builtin args = wrapError' ("while calling builtin " ++ formatBuiltinIsolated builtin) $ do
  case (builtin, args) of
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
    -- matrix functions
    (MatAp _ _, [f, x]) -> valueFromVector <$> (matap <$> valueToMatrix f <*> valueToVector x)
    (MatZero n, []) -> return $ valueFromMatrix (matzero n)
    (MatOne n, []) -> return $ valueFromMatrix (matone n)
    (MatAdd _ _, [f, g]) -> valueFromMatrix <$> (matadd <$> valueToMatrix f <*> valueToMatrix g)
    (MatMul _ _ _, [f, g]) -> valueFromMatrix <$> (matmul <$> valueToMatrix f <*> valueToMatrix g)
    (MatPow _, [f, ValInt k]) -> valueFromMatrix <$> (matpow <$> valueToMatrix f <*> pure k)
    -- modular functions
    (ModInv, [ValInt x, ValInt m]) -> ValInt <$> modinv x m
    (ModPow, [ValInt x, ValInt k, ValInt m]) -> ValInt <$> modpow x k m
    (ModMatAp _ _, [f, x, ValInt m]) -> valueFromModVector <$> (matap <$> valueToModMatrix m f <*> valueToModVector m x)
    (ModMatAdd _ _, [f, g, ValInt m]) -> valueFromModMatrix <$> (matadd <$> valueToModMatrix m f <*> valueToModMatrix m g)
    (ModMatMul _ _ _, [f, g, ValInt m]) -> valueFromModMatrix <$> (matmul <$> valueToModMatrix m f <*> valueToModMatrix m g)
    (ModMatPow _, [f, ValInt k, ValInt m]) -> valueFromModMatrix <$> (matpow <$> valueToModMatrix m f <*> pure k)
    -- list functions
    (Cons _, [x, ValList xs]) -> return $ ValList (V.cons x xs)
    (Foldl _ _, [f, x, ValList a]) -> V.foldM (\x y -> callValue f [x, y]) x a
    (Scanl _ _, [f, x, ValList a]) -> ValList <$> scanM (\x y -> callValue f [x, y]) x a
    (Len _, [ValList a]) -> return $ ValInt (fromIntegral (V.length a))
    (Tabulate _, [ValInt n, f]) -> ValList <$> tabulate n f
    (Map _ _, [f, ValList a]) -> ValList <$> map' f a
    (Filter _, [f, ValList a]) -> ValList <$> V.filterM (\x -> (/= ValBool False) <$> callValue f [x]) a -- TODO
    (At _, [ValList a, ValInt n]) -> atEither a n
    (SetAt _, [ValList a, ValInt n, x]) -> ValList <$> setAtEither a n x
    (Elem _, [x, ValList a]) -> return $ ValBool (x `V.elem` a)
    (Sum, [ValList a]) -> ValInt . sum <$> valueToIntList a
    (Product, [ValList a]) -> ValInt . product <$> valueToIntList a
    (ModProduct, [ValList a, ValInt b]) -> ValInt . (`mod` b) . product <$> valueToIntList a
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
    (LessThan _, [a, b]) -> return $ ValBool (compareValues a b == Just LT)
    (LessEqual _, [a, b]) -> return $ ValBool (compareValues a b /= Just GT)
    (GreaterThan _, [a, b]) -> return $ ValBool (compareValues a b == Just GT)
    (GreaterEqual _, [a, b]) -> return $ ValBool (compareValues a b /= Just LT)
    (Equal _, [a, b]) -> return $ ValBool (a == b)
    (NotEqual _, [a, b]) -> return $ ValBool (a /= b)
    -- combinational functions
    (Fact, [ValInt n]) -> ValInt <$> fact n
    (Choose, [ValInt n, ValInt r]) -> ValInt <$> choose n r
    (Permute, [ValInt n, ValInt r]) -> ValInt <$> permute n r
    (MultiChoose, [ValInt n, ValInt r]) -> ValInt <$> multichoose n r
    _ -> throwInternalError $ "invalid builtin call: " ++ formatBuiltinIsolated builtin ++ "(" ++ intercalate "," (map formatValue args) ++ ")"

callLambda :: MonadError Error m => Maybe VarName -> Env -> [(VarName, Type)] -> Expr -> [Value] -> m Value
callLambda name env formalArgs body actualArgs = wrapError' ("while calling lambda " ++ maybe "(anonymous)" unVarName name) $ do
  if length formalArgs /= length actualArgs
    then throwInternalError $ "wrong number of arguments for lambda function: expr = " ++ formatExpr (Lam formalArgs body) ++ ", args = (" ++ intercalate ", " (map formatValue actualArgs) ++ ")"
    else case (formalArgs, actualArgs) of
      ([], []) -> evaluateExpr env body
      ((x, _) : formalArgs, val : actualArgs) -> callLambda name ((x, val) : env) formalArgs body actualArgs
      _ -> throwInternalError "wrong number of arguments for lambda function"

callValue :: MonadError Error m => Value -> [Value] -> m Value
callValue f args = case f of
  ValBuiltin builtin -> callBuiltin builtin args
  ValLambda name env args' body -> callLambda name env args' body args
  _ -> throwInternalError $ "call non-function: " ++ formatValue f

evaluateExpr :: MonadError Error m => Env -> Expr -> m Value
evaluateExpr env = \case
  Var x -> case lookup x env of
    Nothing -> throwRuntimeError $ "Internal Error: undefined variable: " ++ unVarName x
    Just val -> return val
  Lit lit -> return $ literalToValue lit
  App f args -> do
    f <- evaluateExpr env f
    args <- mapM (evaluateExpr env) args
    callValue f args
  Lam args body -> return $ ValLambda Nothing env args body
  Let x _ e1 e2 -> do
    v1 <- evaluateExpr env e1
    evaluateExpr ((x, v1) : env) e2

callBuiltinWithTokens :: MonadError Error m => [Token] -> Builtin -> m (Value, [Token])
callBuiltinWithTokens tokens builtin = wrapError' ("while calling builtin " ++ formatBuiltinIsolated builtin) $ do
  case builtinToType builtin of
    FunTy ts _ -> do
      (args, tokens) <- readInputMap ts tokens
      val <- callBuiltin builtin args
      return (val, tokens)
    _ -> throwInternalError "all builtin must be functions"

callLambdaWithTokens :: MonadError Error m => [Token] -> Maybe VarName -> Env -> [(VarName, Type)] -> Expr -> m (Value, [Token])
callLambdaWithTokens tokens name env args body = wrapError' ("while calling lambda " ++ maybe "(anonymous)" unVarName name) $ go tokens env args
  where
    go tokens env args = case args of
      ((x, t) : args) -> do
        (val, tokens) <- readInput t tokens
        go tokens ((x, val) : env) args
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
      ValLambda name env args body -> callLambdaWithTokens tokens name env args body
      _ -> return (val, tokens)

evaluateProgram :: (MonadFix m, MonadError Error m) => [Token] -> Program -> m Value
evaluateProgram tokens prog = do
  (val, tokens) <- evaluateToplevelExpr tokens [] prog
  if null tokens
    then return val
    else throwWrongInputError $ "evaluation succeeds, but unused inputs remain: value = " ++ formatValue val ++ ", tokens = " ++ show tokens

-- -----------------------------------------------------------------------------
-- run

run' :: (MonadFix m, MonadError Error m) => [Token] -> Program -> m Value
run' tokens prog = wrapError' "Jikka.Core.Evaluate.run' failed" $ do
  prog <- MakeEager.run prog
  evaluateProgram tokens prog

run :: (MonadIO m, MonadFix m, MonadError Error m) => Program -> m Value
run prog = do
  contents <- liftIO getContents
  let tokens = tokenize contents
  liftEither $ run' tokens prog
