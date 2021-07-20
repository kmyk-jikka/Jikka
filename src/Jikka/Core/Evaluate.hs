{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Jikka.Core.Evaluate
-- Description : executes the expr of our core language. / core 言語の式を実行します。
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
    callProgram,
    Value (..),
  )
where

import Control.Monad.Except
import Data.Bits
import Data.List (maximumBy, minimumBy, sortBy)
import qualified Data.Vector as V
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.Matrix
import Jikka.Core.Format (formatBuiltinIsolated)
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.Runtime
import Jikka.Core.Language.Util
import Jikka.Core.Language.Value

-- -----------------------------------------------------------------------------
-- builtins

iterate' :: MonadError Error m => Integer -> Value -> Value -> m Value
iterate' n _ _ | n < 0 = throwRuntimeError $ "negative number of iteration: " ++ show n
iterate' 0 _ base = return base
iterate' n step base = do
  base <- callValue step [base]
  iterate' (n - 1) step base

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
  let go0 ret f = callValue (ret f) args
  let go1' t1 ret f = case args of
        v1 : args -> do
          f <- ret <$> (f =<< t1 v1)
          callValue f args
        _ -> return $ ValBuiltin builtin args
  let go1 t1 ret f = go1' t1 ret (return . f)
  let go2' t1 t2 ret f = case args of
        v1 : v2 : args -> do
          f <- ret <$> join (f <$> t1 v1 <*> t2 v2)
          callValue f args
        _ -> return $ ValBuiltin builtin args
  let go2 t1 t2 ret f = go2' t1 t2 ret ((return .) . f)
  let go3' t1 t2 t3 ret f = case args of
        v1 : v2 : v3 : args -> do
          f <- ret <$> join (f <$> t1 v1 <*> t2 v2 <*> t3 v3)
          callValue f args
        _ -> return $ ValBuiltin builtin args
  let go3 t1 t2 t3 ret f = go3' t1 t2 t3 ret (((return .) .) . f)
  let goN n t ret f =
        if length args < n
          then return $ ValBuiltin builtin args
          else do
            f <- ret . f <$> mapM t (take n args)
            callValue f (drop n args)
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
    -- advanced arithmetical functions
    Abs -> go1 valueToInt ValInt abs
    Gcd -> go2 valueToInt valueToInt ValInt gcd
    Lcm -> go2 valueToInt valueToInt ValInt lcm
    Min2 _ -> go2 pure pure id minValue
    Max2 _ -> go2 pure pure id maxValue
    Iterate _ -> go3' valueToInt pure pure id $ \n step base -> iterate' n step base
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
    VecFloorMod _ -> go2 valueToVector valueToInt valueFromVector $ \x m -> V.map (`mod` m) x
    MatFloorMod _ _ -> go2 valueToMatrix valueToInt valueFromMatrix $ \f m -> fmap (`mod` m) f
    -- modular functions
    ModNegate -> go2 valueToInt valueToInt ValInt $ \a m -> (- a) `mod` m
    ModPlus -> go3 valueToInt valueToInt valueToInt ValInt $ \a b m -> (a + b) `mod` m
    ModMinus -> go3 valueToInt valueToInt valueToInt ValInt $ \a b m -> (a - b) `mod` m
    ModMult -> go3 valueToInt valueToInt valueToInt ValInt $ \a b m -> (a * b) `mod` m
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
    Map _ _ -> go2' pure valueToList ValList map'
    Filter _ -> go2' pure valueToList ValList $ \f xs -> V.filterM (\x -> (/= ValBool False) <$> callValue f [x]) xs
    At _ -> go2' valueToList valueToInt id atEither
    SetAt _ -> go3' valueToList valueToInt pure ValList setAtEither
    Elem _ -> go2 pure valueToList ValBool V.elem
    Sum -> go1 valueToIntList ValInt sum
    ModSum -> go2 valueToIntList valueToInt ValInt $ \xs m -> sum xs `mod` m
    Product -> go1 valueToIntList ValInt product
    ModProduct -> go2 valueToIntList valueToInt ValInt $ \xs m -> product xs `mod` m
    Min1 _ -> go1 valueToList id (V.minimumBy compareValues')
    Max1 _ -> go1 valueToList id (V.maximumBy compareValues')
    ArgMin _ -> go1 valueToList ValInt $ \xs -> snd (minimumBy (\(x, i) (y, j) -> compareValues' x y <> compare i j) (zip (V.toList xs) [0 ..]))
    ArgMax _ -> go1 valueToList ValInt $ \xs -> snd (maximumBy (\(x, i) (y, j) -> compareValues' x y <> compare i j) (zip (V.toList xs) [0 ..]))
    All -> go1 valueToBoolList ValBool and
    Any -> go1 valueToBoolList ValBool or
    Sorted _ -> go1 valueToList ValList sortVector
    Reversed _ -> go1 valueToList ValList V.reverse
    Range1 -> go1' valueToInt ValList range1
    Range2 -> go2' valueToInt valueToInt ValList range2
    Range3 -> go3' valueToInt valueToInt valueToInt ValList range3
    -- tuple functions
    Tuple ts -> goN (length ts) pure ValTuple id
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

callLambda :: MonadError Error m => Maybe VarName -> Env -> VarName -> Type -> Expr -> [Value] -> m Value
callLambda = \name env x t body args -> wrapError' ("while calling lambda " ++ maybe "(anonymous)" unVarName name) $ go Nothing env x t body args
  where
    go name env x t body [] = return $ ValLambda name env x t body
    go name env x _ body (e : args) = maybe id (\name -> wrapError' $ "while calling lambda " ++ unVarName name) name $ do
      body <- evaluateExpr ((x, e) : env) body
      case body of
        ValLambda name env x t body -> go name env x t body args
        _ -> callValue body args

callValue :: MonadError Error m => Value -> [Value] -> m Value
callValue f args = case (f, args) of
  (ValBuiltin builtin args', _) -> callBuiltin builtin (args' ++ args)
  (ValLambda name env x t body, _) -> callLambda name env x t body args
  (_, []) -> return f
  _ -> throwInternalError $ "cannot call a non-function: " ++ formatValue f

evaluateExpr :: MonadError Error m => Env -> Expr -> m Value
evaluateExpr env = \case
  Var x -> case lookup x env of
    Nothing -> throwInternalError $ "undefined variable: " ++ unVarName x
    Just val -> return val
  Lit lit -> literalToValue lit
  If' _ p e1 e2 -> do
    p <- valueToBool =<< evaluateExpr env p
    if p
      then evaluateExpr env e1
      else evaluateExpr env e2
  e@(App _ _) -> do
    let (f, args) = curryApp e
    f <- evaluateExpr env f
    args <- mapM (evaluateExpr env) args
    callValue f args
  Lam x t body -> return $ ValLambda Nothing env x t body
  Let x _ e1 e2 -> do
    v1 <- evaluateExpr env e1
    evaluateExpr ((x, v1) : env) e2

callToplevelExpr :: (MonadFix m, MonadError Error m) => Env -> ToplevelExpr -> [Value] -> m Value
callToplevelExpr env e args = case e of
  ToplevelLet x _ e cont -> do
    val <- evaluateExpr env e
    callToplevelExpr ((x, val) : env) cont args
  ToplevelLetRec f args' _ body cont -> do
    val <- mfix $ \val -> evaluateExpr ((f, val) : env) (curryLam args' body)
    callToplevelExpr ((f, val) : env) cont args
  ResultExpr e -> do
    val <- evaluateExpr env e
    callValue val args

-- | `callProgram` evaluates programs with given arguments.
-- This function assumes that given programs are ready for eager evaluation (@ensureEagerlyEvaluatable@).
callProgram :: (MonadFix m, MonadError Error m) => Program -> [Value] -> m Value
callProgram prog args = wrapError' "Jikka.Core.Evaluate" $ do
  precondition $ do
    ensureEagerlyEvaluatable prog
    ensureWellTyped prog
  callToplevelExpr [] prog args

-- -----------------------------------------------------------------------------
-- run

run :: (MonadAlpha m, MonadFix m, MonadError Error m) => Program -> [Value] -> m Value
run prog args = do
  callProgram prog args
