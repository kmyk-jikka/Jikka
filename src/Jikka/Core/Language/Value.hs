{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Language.Value where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Jikka.Common.Error
import Jikka.Common.Matrix
import Jikka.Common.ModInt
import Jikka.Core.Format (formatBuiltinIsolated, formatExpr)
import Jikka.Core.Language.Expr

data Value
  = ValInt Integer
  | ValBool Bool
  | ValList (V.Vector Value)
  | ValTuple [Value]
  | ValBuiltin Builtin [Value]
  | -- | The `Env` may contain the `ValLambda` cyclicly.
    ValLambda (Maybe VarName) Env VarName Type Expr
  deriving (Eq, Read)

type Env = [(VarName, Value)]

literalToValue :: Literal -> Value
literalToValue = \case
  LitBuiltin builtin -> ValBuiltin builtin []
  LitInt n -> ValInt n
  LitBool p -> ValBool p
  LitNil _ -> ValList V.empty

valueToInt :: MonadError Error m => Value -> m Integer
valueToInt = \case
  ValInt n -> return n
  val -> throwInternalError $ "not an integer value: " ++ formatValue val

valueToList :: MonadError Error m => Value -> m (V.Vector Value)
valueToList = \case
  ValList xs -> return xs
  val -> throwInternalError $ "not a list value: " ++ formatValue val

valueToIntList :: MonadError Error m => Value -> m [Integer]
valueToIntList xs = mapM valueToInt . V.toList =<< valueToList xs

valueToBool :: MonadError Error m => Value -> m Bool
valueToBool = \case
  ValBool p -> return p
  val -> throwInternalError $ "not an boolean value: " ++ formatValue val

valueToBoolList :: MonadError Error m => Value -> m [Bool]
valueToBoolList xs = mapM valueToBool . V.toList =<< valueToList xs

valueToTuple :: MonadError Error m => Value -> m [Value]
valueToTuple = \case
  ValTuple xs -> return xs
  val -> throwInternalError $ "not a tuple value: " ++ formatValue val

valueToVector :: MonadError Error m => Value -> m (V.Vector Integer)
valueToVector = \case
  ValTuple x -> V.fromList <$> mapM valueToInt x
  val -> throwInternalError $ "not a vector: " ++ formatValue val

valueToMatrix :: MonadError Error m => Value -> m (Matrix Integer)
valueToMatrix a = do
  a <- V.mapM valueToVector . V.fromList =<< valueToTuple a
  case makeMatrix a of
    Just a -> return a
    Nothing -> throwInternalError $ "not a matrix: " ++ show a

valueFromVector :: V.Vector Integer -> Value
valueFromVector x = ValTuple (map ValInt (V.toList x))

valueFromMatrix :: Matrix Integer -> Value
valueFromMatrix f = ValTuple (map (ValTuple . map ValInt . V.toList) (V.toList (unMatrix f)))

valueToModVector :: MonadError Error m => Integer -> Value -> m (V.Vector ModInt)
valueToModVector m x = V.map (`toModInt` m) <$> valueToVector x

valueToModMatrix :: MonadError Error m => Integer -> Value -> m (Matrix ModInt)
valueToModMatrix m f = fmap (`toModInt` m) <$> valueToMatrix f

valueFromModVector :: V.Vector ModInt -> Value
valueFromModVector = valueFromVector . V.map fromModInt

valueFromModMatrix :: Matrix ModInt -> Value
valueFromModMatrix = valueFromMatrix . fmap fromModInt

compareValues :: Value -> Value -> Maybe Ordering
compareValues a b = case (a, b) of
  (ValInt a, ValInt b) -> Just (compare a b)
  (ValBool a, ValBool b) -> Just (compare a b)
  (ValList a, ValList b) -> case mconcat <$> zipWithM compareValues (V.toList a) (V.toList b) of
    Just EQ -> Just (compare (V.length a) (V.length b))
    ordering -> ordering
  (ValTuple a, ValTuple b) -> mconcat <$> zipWithM compareValues a b
  (_, _) -> Nothing

compareValues' :: Value -> Value -> Ordering
compareValues' a b = fromMaybe EQ (compareValues a b)

minValue :: Value -> Value -> Value
minValue a b = if compareValues' a b == LT then a else b

maxValue :: Value -> Value -> Value
maxValue a b = if compareValues' a b == GT then a else b

formatValue :: Value -> String
formatValue = \case
  ValInt n -> show n
  ValBool p -> map toLower (show p)
  ValList xs -> "[" ++ intercalate ", " (map formatValue (V.toList xs)) ++ "]"
  ValTuple [x] -> "(" ++ formatValue x ++ ",)"
  ValTuple xs -> "(" ++ intercalate ", " (map formatValue xs) ++ ")"
  ValBuiltin builtin [] -> formatBuiltinIsolated builtin
  ValBuiltin builtin args -> formatBuiltinIsolated builtin ++ "(" ++ intercalate ", " (map formatValue args) ++ ")"
  ValLambda _ _ x t body -> formatExpr (Lam x t body) -- Don't show env because it may be cyclic.

writeValueIO :: Value -> IO ()
writeValueIO = \case
  ValInt n -> print n
  ValBool p -> putStrLn (if p then "Yes" else "No")
  ValList xs -> do
    print (V.length xs)
    mapM_ writeValueIO xs
  ValTuple xs -> mapM_ writeValueIO xs
  f@(ValBuiltin _ _) -> putStrLn (formatValue f)
  f@ValLambda {} -> putStrLn (formatValue f)
