{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Language.Value where

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
  deriving (Eq, Ord, Show, Read)

newtype Local = Local
  { unLocal :: M.Map VarName Value
  }
  deriving (Eq, Ord, Show, Read)

data Builtin
  = BuiltinUnsupported
  | BuiltinAbs
  | BuiltinAll
  | BuiltinMin
  | BuiltinAny
  | BuiltinDivMod
  | BuiltinSorted
  | BuiltinEnumerate
  | BuiltinBool
  | BuiltinInt
  | BuiltinSum
  | BuiltinZip
  | BuiltinFilter
  | BuiltinTuple
  | BuiltinLen
  | BuiltinList
  | BuiltinRange
  | BuiltinMap
  | BuiltinReversed
  | BuiltinMax
  | BuiltinArgMax
  | BuiltinArgMin
  | BuiltinCeilDiv
  | BuiltinCeilMod
  | BuiltinChoose
  | BuiltinFact
  | BuiltinFloorDiv
  | BuiltinFloorMod
  | BuiltinGcd
  | BuiltinInv
  | BuiltinLcm
  | BuiltinMultiChoose
  | BuiltinPermute
  | BuiltinProduct
  deriving (Eq, Ord, Show, Read)

standardBuiltinFunctions :: M.Map VarName Builtin
standardBuiltinFunctions =
  M.fromList
    [ ("abs", BuiltinAbs),
      ("delattr", BuiltinUnsupported),
      ("hash", BuiltinUnsupported),
      ("memoryview", BuiltinUnsupported),
      ("set", BuiltinUnsupported),
      ("all", BuiltinAll),
      ("dict", BuiltinUnsupported),
      ("help", BuiltinUnsupported),
      ("min", BuiltinMin),
      ("setattr", BuiltinUnsupported),
      ("any", BuiltinAny),
      ("dir", BuiltinUnsupported),
      ("hex", BuiltinUnsupported),
      ("next", BuiltinUnsupported),
      ("slice", BuiltinUnsupported),
      ("ascii", BuiltinUnsupported),
      ("divmod", BuiltinDivMod),
      ("id", BuiltinUnsupported),
      ("object", BuiltinUnsupported),
      ("sorted", BuiltinSorted),
      ("bin", BuiltinUnsupported),
      ("enumerate", BuiltinEnumerate),
      ("input", BuiltinUnsupported),
      ("oct", BuiltinUnsupported),
      ("staticmethod", BuiltinUnsupported),
      ("bool", BuiltinBool),
      ("eval", BuiltinUnsupported),
      ("int", BuiltinInt),
      ("open", BuiltinUnsupported),
      ("str", BuiltinUnsupported),
      ("breakpoint", BuiltinUnsupported),
      ("exec", BuiltinUnsupported),
      ("isinstance", BuiltinUnsupported),
      ("ord", BuiltinUnsupported),
      ("sum", BuiltinSum),
      ("bytearray", BuiltinUnsupported),
      ("filter", BuiltinFilter),
      ("issubclass", BuiltinUnsupported),
      ("pow", BuiltinUnsupported),
      ("super", BuiltinUnsupported),
      ("bytes", BuiltinUnsupported),
      ("float", BuiltinUnsupported),
      ("iter", BuiltinUnsupported),
      ("print", BuiltinUnsupported),
      ("tuple", BuiltinUnsupported),
      ("callable", BuiltinUnsupported),
      ("format", BuiltinUnsupported),
      ("len", BuiltinLen),
      ("property", BuiltinUnsupported),
      ("type", BuiltinUnsupported),
      ("chr", BuiltinUnsupported),
      ("frozenset", BuiltinUnsupported),
      ("list", BuiltinList),
      ("range", BuiltinRange),
      ("vars", BuiltinUnsupported),
      ("classmethod", BuiltinUnsupported),
      ("getattr", BuiltinUnsupported),
      ("locals", BuiltinUnsupported),
      ("repr", BuiltinUnsupported),
      ("zip", BuiltinZip),
      ("compile", BuiltinUnsupported),
      ("globals", BuiltinUnsupported),
      ("map", BuiltinMap),
      ("reversed", BuiltinReversed),
      ("__import__", BuiltinUnsupported),
      ("complex", BuiltinUnsupported),
      ("hasattr", BuiltinUnsupported),
      ("max", BuiltinMax),
      ("round", BuiltinUnsupported)
    ]

additionalBuiltinFunctions :: M.Map VarName Builtin
additionalBuiltinFunctions =
  M.fromList
    [ ("argmax", BuiltinArgMax),
      ("argmin", BuiltinArgMin),
      ("ceildiv", BuiltinCeilDiv),
      ("ceilmod", BuiltinCeilMod),
      ("choose", BuiltinChoose),
      ("fact", BuiltinFact),
      ("floordiv", BuiltinFloorDiv),
      ("floormod", BuiltinFloorMod),
      ("gcd", BuiltinGcd),
      ("inv", BuiltinInv),
      ("lcm", BuiltinLcm),
      ("multichoose", BuiltinMultiChoose),
      ("permute", BuiltinPermute),
      ("product", BuiltinProduct)
    ]

toIntList :: V.Vector Value -> Maybe (V.Vector Integer)
toIntList xs = mapM go xs
  where
    go (IntVal x) = Just x
    go _ = Nothing

toIntList' :: MonadError Error m => V.Vector Value -> m (V.Vector Integer)
toIntList' xs = case toIntList xs of
  Just xs -> return xs
  Nothing -> throwRuntimeError "not a list of integers"

toBoolList' :: MonadError Error m => V.Vector Value -> m (V.Vector Bool)
toBoolList' xs = mapM go xs
  where
    go (BoolVal x) = return x
    go _ = throwRuntimeError "not a list of booleans"

toMatrix :: V.Vector Value -> Maybe (Matrix Integer)
toMatrix a = makeMatrix =<< mapM go a
  where
    go (ListVal row) = toIntList row
    go _ = Nothing

toMatrix' :: MonadError Error m => V.Vector Value -> m (Matrix Integer)
toMatrix' a = case toMatrix a of
  Just a -> return a
  Nothing -> throwRuntimeError "not a matrix"

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
initialGlobal = Global $ M.map BuiltinVal (M.union standardBuiltinFunctions additionalBuiltinFunctions)

lookupGlobal :: MonadError Error m => VarName -> Global -> m Value
lookupGlobal x global =
  case M.lookup x (unGlobal global) of
    Just y -> return y
    Nothing -> throwSymbolError $ "undefined variable: " ++ unVarName x

makeEntryPointIO :: (MonadIO m, MonadError Error m) => VarName -> Global -> m Expr
makeEntryPointIO f global = do
  v <- lookupGlobal f global
  case v of
    ClosureVal _ args _ -> do
      args <- mapM (readValueIO . snd) args
      return $ Call (Name f) args
    _ -> throwSymbolError $ "not a function: " ++ unVarName f

writeValueIO :: Value -> IO ()
writeValueIO = \case
  IntVal n -> print n
  BoolVal p -> print p
  ListVal xs -> mapM_ writeValueIO (V.toList xs)
  TupleVal xs -> mapM_ writeValueIO xs
  f@ClosureVal {} -> print f
  BuiltinVal b -> print b
