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
import qualified Data.Array as A
import Data.Bits
import Data.List (sort)
import Jikka.Common.Language.Name
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint (builtinToType)
import Text.Read (readEither)

-- -----------------------------------------------------------------------------
-- values

data Value
  = ValInt Integer
  | ValBool Bool
  | ValList (A.Array Int Value)
  | ValBuiltin Builtin
  | ValLambda Env [(VarName, Type)] Expr
  deriving (Eq, Ord, Show, Read)

literalToValue :: Literal -> Value
literalToValue = \case
  LitBuiltin builtin -> ValBuiltin builtin
  LitInt n -> ValInt n
  LitBool p -> ValBool p

valueToInt :: Value -> Either String Integer
valueToInt = \case
  ValInt n -> Right n
  val -> Left $ "Runtime Error: Internal Error: not int: " ++ show val

valueToIntList :: A.Array Int Value -> Either String [Integer]
valueToIntList = mapM valueToInt . A.elems

valueToBool :: Value -> Either String Bool
valueToBool = \case
  ValBool p -> Right p
  val -> Left $ "Runtime Error: Internal Error: not bool: " ++ show val

valueToBoolList :: A.Array Int Value -> Either String [Bool]
valueToBoolList = mapM valueToBool . A.elems

-- -----------------------------------------------------------------------------
-- inputs

newtype Token = Token {unToken :: String}
  deriving (Eq, Ord, Show, Read)

tokenize :: String -> [Token]
tokenize = map Token . words

readToken :: Read a => Token -> Either String a
readToken = readEither . unToken

readInputList :: Type -> Int -> [Token] -> Either String ([Value], [Token])
readInputList t = go
  where
    go n _ | n < 0 = throwError "Runtime Error: Wrong Input: negative size of list in input"
    go 0 tokens = return ([], tokens)
    go n tokens = do
      (x, tokens) <- readInput t tokens
      (xs, tokens) <- go (n - 1) tokens
      return (x : xs, tokens)

readInputMap :: [Type] -> [Token] -> Either String ([Value], [Token])
readInputMap ts tokens = case ts of
  [] -> return ([], tokens)
  (t : ts) -> do
    (val, tokens) <- readInput t tokens
    (vals, tokens) <- readInputMap ts tokens
    return (val : vals, tokens)

readInput :: Type -> [Token] -> Either String (Value, [Token])
readInput t tokens = case (t, tokens) of
  (_, []) -> throwError "Runtime Error: Wrong Input: it reaches EOF"
  (IntTy, token : tokens) -> do
    n <- readToken token
    return (ValInt n, tokens)
  (BoolTy, token : tokens) -> do
    p <- readToken token
    return (ValBool p, tokens)
  (ListTy t, token : tokens) -> do
    n <- readToken token
    (a, tokens) <- readInputList t n tokens
    let a' = A.listArray (0, n - 1) a
    return (ValList a', tokens)
  (FunTy _ _, _) -> throwError "Runtime Error: Wrong Input: we cannot use functions as inputs"

-- -----------------------------------------------------------------------------
-- builtins

floorDiv :: Integer -> Integer -> Either String Integer
floorDiv _ 0 = throwError "Runtime Error: zero div"
floorDiv a b = return (a `div` b)

floorMod :: Integer -> Integer -> Either String Integer
floorMod _ 0 = throwError "Runtime Error: zero div"
floorMod a b = return (a `mod` b)

ceilDiv :: Integer -> Integer -> Either String Integer
ceilDiv _ 0 = throwError "Runtime Error: zero div"
ceilDiv a b = return ((a + b - 1) `div` b)

ceilMod :: Integer -> Integer -> Either String Integer
ceilMod _ 0 = throwError "Runtime Error: zero div"
ceilMod a b = return (a - ((a + b - 1) `div` b) * b)

natind :: Value -> Value -> Integer -> Either String Value
natind _ _ n | n < 0 = throwError $ "Runtime Error: negative number for mathematical induction: " ++ show n
natind base _ 0 = return base
natind base step n = do
  val <- natind base step (n - 1)
  callValue step [val, ValInt (n - 1)]

minimumEither :: Ord a => [a] -> Either String a
minimumEither [] = throwError "Runtime Error: there is no minimum for the empty list"
minimumEither a = return $ minimum a

maximumEither :: Ord a => [a] -> Either String a
maximumEither [] = throwError "Runtime Error: there is no maximum for the empty list"
maximumEither a = return $ maximum a

argminEither :: Ord a => [a] -> Either String Integer
argminEither [] = throwError "Runtime Error: there is no minimum for the empty list"
argminEither a = return $ snd (minimum (zip a [0 ..]))

argmaxEither :: Ord a => [a] -> Either String Integer
argmaxEither [] = throwError "Runtime Error: there is no maximum for the empty list"
argmaxEither a = return $ snd (maximum (zip a [0 ..]))

inv :: Integer -> Integer -> Either String Integer
inv a m | m <= 0 || a `mod` m == 0 = throwError $ "Runtime Error: invalid argument for inv: " ++ show (a, m)
inv _ _ = error "TODO: implement this"

powmod :: Integer -> Integer -> Integer -> Either String Integer
powmod _ _ m | m <= 0 = throwError $ "Runtime Error: invalid argument for powmod: MOD = " ++ show m
powmod a b m = return $ (a ^ b) `mod` m

listToArray :: [a] -> A.Array Int a
listToArray xs = A.listArray (0, length xs) xs

lengthArray :: A.Array Int a -> Integer
lengthArray a =
  let (l, r) = A.bounds a
   in toInteger (r - l + 1)

tabulate :: Integer -> Value -> Either String (A.Array Int Value)
tabulate n f = listToArray <$> mapM (\i -> callValue f [ValInt i]) [0 .. n - 1]

map' :: Value -> A.Array Int Value -> Either String (A.Array Int Value)
map' f a = listToArray <$> mapM (\val -> callValue f [val]) (A.elems a)

atEither :: A.Array Int a -> Integer -> Either String a
atEither a n =
  let (l, r) = A.bounds a
   in if toInteger l <= n && n <= toInteger r
        then return (a A.! fromInteger n)
        else throwError $ "Runtime Error: out of bounds: " ++ show (l, r, n)

sortArray :: Ord a => A.Array Int a -> A.Array Int a
sortArray = listToArray . sort . A.elems

reverseArray :: A.Array Int a -> A.Array Int a
reverseArray = listToArray . reverse . A.elems

range1 :: Integer -> Either String (A.Array Int Value)
range1 n | n < 0 = throwError $ "Runtime Error: invalid argument for range1: " ++ show n
range1 n = return $ listToArray (map ValInt [0 .. n - 1])

range2 :: Integer -> Integer -> Either String (A.Array Int Value)
range2 l r | l > r = throwError $ "Runtime Error: invalid argument for range2: " ++ show (l, r)
range2 l r = return $ listToArray (map ValInt [l .. r - 1])

range3 :: Integer -> Integer -> Integer -> Either String (A.Array Int Value)
range3 l r step | not (l <= r && step >= 0) = throwError $ "Runtime Error: invalid argument for range3: " ++ show (l, r, step)
range3 l r step = return $ listToArray (map ValInt [l, l + step .. r])

fact :: Integer -> Either String Integer
fact n | n < 0 = throwError $ "Runtime Error: invalid argument for fact: " ++ show n
fact n = return $ product [1 .. n]

choose :: Integer -> Integer -> Either String Integer
choose n r | not (0 <= r && r <= n) = throwError $ "Runtime Error: invalid argument for choose: " ++ show (n, r)
choose n r = return $ product [n - r + 1 .. n] `div` product [1 .. r]

permute :: Integer -> Integer -> Either String Integer
permute n r | not (0 <= r && r <= n) = throwError $ "Runtime Error: invalid argument for choose: " ++ show (n, r)
permute n r = return $ product [n - r + 1 .. n]

multichoose :: Integer -> Integer -> Either String Integer
multichoose n r | not (0 <= r && r <= n) = throwError $ "Runtime Error: invalid argument for multichoose: " ++ show (n, r)
multichoose 0 0 = return 1
multichoose n r = choose (n + r - 1) r

-- -----------------------------------------------------------------------------
-- evaluator

type Env = [(VarName, Value)]

callBuiltin :: Builtin -> [Value] -> Either String Value
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
  (Min, [ValInt a, ValInt b]) -> return $ ValInt (min a b)
  (Max, [ValInt a, ValInt b]) -> return $ ValInt (max a b)
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
  (Inv, [ValInt a, ValInt b]) -> ValInt <$> inv a b
  (PowMod, [ValInt a, ValInt b, ValInt c]) -> ValInt <$> powmod a b c
  -- list functions
  (Len _, [ValList a]) -> return $ ValInt (lengthArray a)
  (Tabulate _, [ValInt n, f]) -> ValList <$> tabulate n f
  (Map _ _, [f, ValList a]) -> ValList <$> map' f a
  (At _, [ValList a, ValInt n]) -> atEither a n
  (Sum, [ValList a]) -> ValInt . sum <$> valueToIntList a
  (Product, [ValList a]) -> ValInt . product <$> valueToIntList a
  (Min1, [ValList a]) -> ValInt <$> (minimumEither =<< valueToIntList a)
  (Max1, [ValList a]) -> ValInt <$> (maximumEither =<< valueToIntList a)
  (ArgMin, [ValList a]) -> ValInt <$> (argminEither =<< valueToIntList a)
  (ArgMax, [ValList a]) -> ValInt <$> (argmaxEither =<< valueToIntList a)
  (All, [ValList a]) -> ValBool . and <$> valueToBoolList a
  (Any, [ValList a]) -> ValBool . or <$> valueToBoolList a
  (Sorted _, [ValList a]) -> return $ ValList (sortArray a)
  (List _, [ValList a]) -> return $ ValList a
  (Reversed _, [ValList a]) -> return $ ValList (reverseArray a)
  (Range1, [ValInt n]) -> ValList <$> range1 n
  (Range2, [ValInt l, ValInt r]) -> ValList <$> range2 l r
  (Range3, [ValInt l, ValInt r, ValInt step]) -> ValList <$> range3 l r step
  -- arithmetical relations
  (LessThan, [ValInt a, ValInt b]) -> return $ ValBool (a < b)
  (LessEqual, [ValInt a, ValInt b]) -> return $ ValBool (a <= b)
  (GreaterThan, [ValInt a, ValInt b]) -> return $ ValBool (a > b)
  (GreaterEqual, [ValInt a, ValInt b]) -> return $ ValBool (a >= b)
  -- equality relations (polymorphic)
  (Equal _, [a, b]) -> return $ ValBool (a == b)
  (NotEqual _, [a, b]) -> return $ ValBool (a /= b)
  -- combinational functions
  (Fact, [ValInt n]) -> ValInt <$> fact n
  (Choose, [ValInt n, ValInt r]) -> ValInt <$> choose n r
  (Permute, [ValInt n, ValInt r]) -> ValInt <$> permute n r
  (MultiChoose, [ValInt n, ValInt r]) -> ValInt <$> multichoose n r
  _ -> throwError $ "Runtime Error: Internal Error: invalid builtin call: " ++ show (builtin, args)

callLambda :: Env -> [(VarName, Type)] -> Expr -> [Value] -> Either String Value
callLambda env formalArgs body actualArgs = case (formalArgs, actualArgs) of
  ([], []) -> evaluateExpr env body
  ((x, _) : formalArgs, val : actualArgs) -> callLambda ((x, val) : env) formalArgs body actualArgs
  _ -> Left "Runtime Error: Internal Error: wrong number of arguments for lambda function"

callValue :: Value -> [Value] -> Either String Value
callValue f args = case f of
  ValBuiltin builtin -> callBuiltin builtin args
  ValLambda env args' body -> callLambda env args' body args
  _ -> Left $ "Runtime Error: Internal Error: call non-function: " ++ show f

evaluateExpr :: Env -> Expr -> Either String Value
evaluateExpr env = \case
  Var x -> case lookup x env of
    Nothing -> throwError $ "Runtime Error: Internal Error: undefined variable: " ++ show (unVarName x)
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

callBuiltinWithTokens :: [Token] -> Builtin -> Either String (Value, [Token])
callBuiltinWithTokens tokens builtin = do
  case builtinToType builtin of
    FunTy ts _ -> do
      (args, tokens) <- readInputMap ts tokens
      val <- callBuiltin builtin args
      return (val, tokens)
    _ -> throwError "Runtime Error: Internal Error: all builtin are functions"

callLambdaWithTokens :: [Token] -> Env -> [(VarName, Type)] -> Expr -> Either String (Value, [Token])
callLambdaWithTokens tokens env args body = case args of
  ((x, t) : args) -> do
    (val, tokens) <- readInput t tokens
    callLambdaWithTokens tokens ((x, val) : env) args body
  [] -> do
    val <- evaluateExpr env body
    return (val, tokens)

evaluateToplevelExpr :: [Token] -> Env -> ToplevelExpr -> Either String (Value, [Token])
evaluateToplevelExpr tokens env = \case
  ToplevelLet rec f args _ body cont -> do
    val <- case rec of
      NonRec -> evaluateExpr env (Lam args body)
      Rec -> mfix $ \val -> evaluateExpr ((f, val) : env) (Lam args body)
    evaluateToplevelExpr tokens ((f, val) : env) cont
  ResultExpr e -> do
    val <- evaluateExpr env e
    case val of
      ValBuiltin builtin -> callBuiltinWithTokens tokens builtin
      ValLambda env args body -> callLambdaWithTokens tokens env args body
      _ -> return (val, tokens)

evaluateProgram :: [Token] -> Program -> Either String Value
evaluateProgram tokens prog = do
  (val, tokens) <- evaluateToplevelExpr tokens [] prog
  if null tokens
    then return val
    else throwError $ "Runtime Error: Wrong Input: evaluation succeeds, but unused inputs remain: " ++ show (val, tokens)

-- -----------------------------------------------------------------------------
-- run

run' :: [Token] -> Program -> Either String Value
run' = evaluateProgram

run :: Program -> ExceptT String IO Value
run prog = do
  contents <- liftIO getContents
  let tokens = tokenize contents
  liftEither $ evaluateProgram tokens prog
