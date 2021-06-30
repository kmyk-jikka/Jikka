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
import Data.List (intercalate, maximumBy, minimumBy, sortBy)
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

matap' :: (Num a, MonadError Error m) => Matrix a -> V.Vector a -> m (V.Vector a)
matap' f x | snd (matsize f) /= V.length x = throwInternalError "invalid argument"
matap' f x = return $ matap f x

matadd' :: (Num a, MonadError Error m) => Matrix a -> Matrix a -> m (Matrix a)
matadd' f g | matsize f /= matsize g = throwInternalError "invalid argument"
matadd' f g = return $ matadd f g

matmul' :: (Num a, MonadError Error m) => Matrix a -> Matrix a -> m (Matrix a)
matmul' f g | snd (matsize f) /= fst (matsize g) = throwInternalError "invalid argument"
matmul' f g = return $ matmul f g

matpow' :: (Num a, Show a, MonadError Error m) => Matrix a -> Integer -> m (Matrix a)
matpow' f _ | let (h, w) = matsize f in h /= w = throwInternalError $ "matrix is not square: " ++ show (matsize f)
matpow' _ k | k < 0 = throwRuntimeError $ "exponent is negative: " ++ show k
matpow' f k = return $ matpow f k

-- -----------------------------------------------------------------------------
-- evaluator

callBuiltin :: MonadError Error m => Builtin -> [Value] -> m Value
callBuiltin builtin args = wrapError' ("while calling builtin " ++ formatBuiltinIsolated builtin) $ do
  let go0 ret f = case args of
        [] -> return $ ret f
        _ -> throwInternalError $ "expected 0 arguments, got " ++ show (length args)
  let go1' t1 ret f = case args of
        [v1] -> ret <$> (f =<< t1 v1)
        _ -> throwInternalError $ "expected 1 argument, got " ++ show (length args)
  let go1 t1 ret f = go1' t1 ret (return . f)
  let go2' t1 t2 ret f = case args of
        [v1, v2] -> ret <$> join (f <$> t1 v1 <*> t2 v2)
        _ -> throwInternalError $ "expected 2 arguments, got " ++ show (length args)
  let go2 t1 t2 ret f = go2' t1 t2 ret ((return .) . f)
  let go3' t1 t2 t3 ret f = case args of
        [v1, v2, v3] -> ret <$> join (f <$> t1 v1 <*> t2 v2 <*> t3 v3)
        _ -> throwInternalError $ "expected 3 arguments, got " ++ show (length args)
  let go3 t1 t2 t3 ret f = go3' t1 t2 t3 ret (((return .) .) . f)
  let goN t ret f = ret . f <$> mapM t args
  case builtin of
    -- arithmetical functions
    Negate -> go1 valueToInt ValInt negate
    Plus -> go2 valueToInt valueToInt ValInt (+)
    Minus -> go2 valueToInt valueToInt ValInt (-)
    Mult -> go2 valueToInt valueToInt ValInt (*)
    FloorDiv -> go2' valueToInt valueToInt ValInt floorDiv
    FloorMod -> go2' valueToInt valueToInt ValInt floorMod
    CeilDiv -> go2' valueToInt valueToInt ValInt ceilDiv
    CeilMod -> go2' valueToInt valueToInt ValInt ceilMod
    Pow -> go2 valueToInt valueToInt ValInt (^)
    -- induction functions
    NatInd _ -> go3' pure pure valueToInt id $ \base step n -> natind base step n
    -- advanced arithmetical functions
    Abs -> go1 valueToInt ValInt abs
    Gcd -> go2 valueToInt valueToInt ValInt gcd
    Lcm -> go2 valueToInt valueToInt ValInt lcm
    Min2 _ -> go2 pure pure id minValue
    Max2 _ -> go2 pure pure id maxValue
    -- logical functions
    Not -> go1 valueToBool ValBool not
    And -> go2 valueToBool valueToBool ValBool (&&)
    Or -> go2 valueToBool valueToBool ValBool (||)
    Implies -> go2 valueToBool valueToBool ValBool $ \p q -> not p || q
    If _ -> go3 valueToBool pure pure id $ \p a b -> if p then a else b
    -- bitwise functions
    BitNot -> go1 valueToInt ValInt complement
    BitAnd -> go2 valueToInt valueToInt ValInt (.&.)
    BitOr -> go2 valueToInt valueToInt ValInt (.|.)
    BitXor -> go2 valueToInt valueToInt ValInt xor
    BitLeftShift -> go2 valueToInt valueToInt ValInt $ \a b -> a `shift` fromInteger b
    BitRightShift -> go2 valueToInt valueToInt ValInt $ \a b -> a `shift` fromInteger (- b)
    -- matrix functions
    MatAp _ _ -> go2' valueToMatrix valueToVector valueFromVector matap'
    MatZero n -> go0 valueFromMatrix (matzero n)
    MatOne n -> go0 valueFromMatrix (matone n)
    MatAdd _ _ -> go2' valueToMatrix valueToMatrix valueFromMatrix matadd'
    MatMul _ _ _ -> go2' valueToMatrix valueToMatrix valueFromMatrix matmul'
    MatPow _ -> go2' valueToMatrix valueToInt valueFromMatrix matpow'
    -- modular functions
    ModInv -> go2' valueToInt valueToInt ValInt modinv
    ModPow -> go3' valueToInt valueToInt valueToInt ValInt modpow
    ModMatAp _ _ -> go3' pure pure valueToInt valueFromModVector $ \f x m -> join (matap' <$> valueToModMatrix m f <*> valueToModVector m x)
    ModMatAdd _ _ -> go3' pure pure valueToInt valueFromModMatrix $ \f g m -> join (matadd' <$> valueToModMatrix m f <*> valueToModMatrix m g)
    ModMatMul _ _ _ -> go3' pure pure valueToInt valueFromModMatrix $ \f g m -> join (matmul' <$> valueToModMatrix m f <*> valueToModMatrix m g)
    ModMatPow _ -> go3' pure valueToInt valueToInt valueFromModMatrix $ \f k m -> join (matpow' <$> valueToModMatrix m f <*> pure k)
    -- list functions
    Cons _ -> go2 pure valueToList ValList V.cons
    Foldl _ _ -> go3' pure pure valueToList id $ \f x a -> V.foldM (\x y -> callValue f [x, y]) x a
    Scanl _ _ -> go3' pure pure valueToList ValList $ \f x a -> scanM (\x y -> callValue f [x, y]) x a
    Len _ -> go1 valueToList ValInt (fromIntegral . V.length)
    Tabulate _ -> go2' valueToInt pure ValList tabulate
    Map _ _ -> go2' pure valueToList ValList map'
    Filter _ -> go2' pure valueToList ValList $ \f xs -> V.filterM (\x -> (/= ValBool False) <$> callValue f [x]) xs
    At _ -> go2' valueToList valueToInt id atEither
    SetAt _ -> go3' valueToList valueToInt pure ValList setAtEither
    Elem _ -> go2 pure valueToList ValBool V.elem
    Sum -> go1 valueToIntList ValInt sum
    Product -> go1 valueToIntList ValInt product
    ModProduct -> go2 valueToIntList valueToInt ValInt $ \xs m -> product xs `mod` m
    Min1 _ -> go1 valueToList id (V.minimumBy compareValues')
    Max1 _ -> go1 valueToList id (V.maximumBy compareValues')
    ArgMin _ -> go1 valueToList ValInt $ \xs -> snd (minimumBy (\(x, i) (y, j) -> compareValues' x y <> compare i j) (zip (V.toList xs) [0 ..]))
    ArgMax _ -> go1 valueToList ValInt $ \xs -> snd (maximumBy (\(x, i) (y, j) -> compareValues' x y <> compare i j) (zip (V.toList xs) [0 ..]))
    All -> go1 valueToBoolList ValBool and
    Any -> go1 valueToBoolList ValBool or
    Sorted _ -> go1 valueToList ValList sortVector
    List _ -> go1 valueToList ValList id
    Reversed _ -> go1 valueToList ValList V.reverse
    Range1 -> go1' valueToInt ValList range1
    Range2 -> go2' valueToInt valueToInt ValList range2
    Range3 -> go3' valueToInt valueToInt valueToInt ValList range3
    -- tuple functions
    Tuple _ -> goN pure ValTuple id
    Proj _ n -> go1 valueToTuple id (!! n)
    -- -- comparison
    LessThan _ -> go2 pure pure ValBool $ \a b -> compareValues a b == Just LT
    LessEqual _ -> go2 pure pure ValBool $ \a b -> compareValues a b /= Just GT
    GreaterThan _ -> go2 pure pure ValBool $ \a b -> compareValues a b == Just GT
    GreaterEqual _ -> go2 pure pure ValBool $ \a b -> compareValues a b /= Just LT
    Equal _ -> go2 pure pure ValBool (==)
    NotEqual _ -> go2 pure pure ValBool (/=)
    -- combinational functions
    Fact -> go1' valueToInt ValInt fact
    Choose -> go2' valueToInt valueToInt ValInt choose
    Permute -> go2' valueToInt valueToInt ValInt permute
    MultiChoose -> go2' valueToInt valueToInt ValInt multichoose

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
