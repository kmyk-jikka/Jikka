{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Language.Value where

import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Jikka.Common.Error
import Jikka.Common.Matrix
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util

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
--         \vert & \mathrm{builtin} \\
--     \end{array}
-- \]
data Value
  = IntVal Integer
  | BoolVal Bool
  | ListVal (V.Vector Value)
  | TupleVal [Value]
  | ClosureVal Local [(VarName, Type)] [Statement]
  | BuiltinVal Builtin
  | AttributeVal Value Attribute
  deriving (Eq, Ord, Show, Read)

newtype Local = Local
  { unLocal :: M.Map VarName Value
  }
  deriving (Eq, Ord, Show, Read)

toInt :: MonadError Error m => Value -> m Integer
toInt = \case
  IntVal n -> return n
  v -> throwInternalError $ "not an integer value: " ++ formatValue v

toBool :: MonadError Error m => Value -> m Bool
toBool = \case
  BoolVal p -> return p
  v -> throwInternalError $ "not a boolean value: " ++ formatValue v

toList :: MonadError Error m => Value -> m (V.Vector Value)
toList = \case
  ListVal xs -> return xs
  v -> throwInternalError $ "not a list value: " ++ formatValue v

toTuple :: MonadError Error m => Value -> m [Value]
toTuple = \case
  TupleVal xs -> return xs
  v -> throwInternalError $ "not a tuple value: " ++ formatValue v

toIntList :: MonadError Error m => Value -> m (V.Vector Integer)
toIntList xs = V.mapM toInt =<< toList xs

toBoolList :: MonadError Error m => Value -> m (V.Vector Bool)
toBoolList xs = V.mapM toBool =<< toList xs

toMatrix :: MonadError Error m => Value -> m (Matrix Integer)
toMatrix a = toMatrix' =<< V.mapM toIntList =<< toList a
  where
    toMatrix' a = case makeMatrix a of
      Just a -> return a
      Nothing -> throwInternalError $ "not a matrix: " ++ show a

fromMatrix :: Matrix Integer -> Value
fromMatrix a = ListVal (fmap (ListVal . fmap IntVal) (unMatrix a))

compareValues :: Value -> Value -> Maybe Ordering
compareValues a b = case (a, b) of
  (IntVal a, IntVal b) -> Just $ compare a b
  (BoolVal a, BoolVal b) -> Just $ compare a b
  (ListVal a, ListVal b) -> case mconcat (V.toList (V.zipWith compareValues a b)) of
    Nothing -> Nothing
    Just EQ -> Just $ compare (V.length a) (V.length b)
    Just o -> Just o
  (TupleVal a, TupleVal b) ->
    if length a /= length b
      then Nothing
      else mconcat (zipWith compareValues a b)
  (_, _) -> Nothing

compareValues' :: Value -> Value -> Ordering
compareValues' a b = fromMaybe EQ (compareValues a b)

newtype Global = Global
  { unGlobal :: M.Map VarName Value
  }
  deriving (Eq, Ord, Show, Read)

initialGlobal :: Global
initialGlobal = Global M.empty

lookupGlobal :: MonadError Error m => VarName' -> Global -> m Value
lookupGlobal x global =
  case M.lookup (value' x) (unGlobal global) of
    Just y -> return y
    Nothing -> throwSymbolErrorAt' (loc' x) $ "undefined variable: " ++ unVarName (value' x)

makeEntryPointIO :: (MonadIO m, MonadError Error m) => VarName' -> Global -> m Expr'
makeEntryPointIO f global = do
  v <- lookupGlobal f global
  withoutLoc <$> case v of
    ClosureVal _ args _ -> do
      args <- mapM (readValueIO . snd) args
      return $ Call (withoutLoc (Name f)) args
    _ -> throwSymbolErrorAt' (loc' f) $ "not a function: " ++ unVarName (value' f)

formatValue :: Value -> String
formatValue = \case
  IntVal n -> show n
  BoolVal p -> map toLower (show p)
  ListVal xs -> "[" ++ intercalate ", " (map formatValue (V.toList xs)) ++ "]"
  TupleVal [x] -> "(" ++ formatValue x ++ ",)"
  TupleVal xs -> "(" ++ intercalate ", " (map formatValue xs) ++ ")"
  f@ClosureVal {} -> show f
  BuiltinVal b -> show b
  AttributeVal x a -> "(" ++ formatValue x ++ ")." ++ show a

writeValueIO :: Value -> IO ()
writeValueIO = \case
  IntVal n -> print n
  BoolVal p -> putStrLn (if p then "Yes" else "No")
  ListVal xs -> do
    print (V.length xs)
    mapM_ writeValueIO (V.toList xs)
  TupleVal xs -> mapM_ writeValueIO xs
  f@ClosureVal {} -> print f
  BuiltinVal b -> print b
  AttributeVal x a -> writeValueIO x >> print a
