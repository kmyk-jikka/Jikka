{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Language.Value where

import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Vector as V
import Jikka.Common.Error
import Jikka.Common.Matrix
import Jikka.Common.ModInt
import Jikka.Core.Language.Expr

data Value
  = ValInt Integer
  | ValBool Bool
  | ValList (V.Vector Value)
  | ValTuple [Value]
  | ValBuiltin Builtin
  | ValLambda (Maybe VarName) Env [(VarName, Type)] Expr
  deriving (Eq, Ord, Show, Read)

type Env = [(VarName, Value)]

literalToValue :: Literal -> Value
literalToValue = \case
  LitBuiltin builtin -> ValBuiltin builtin
  LitInt n -> ValInt n
  LitBool p -> ValBool p
  LitNil _ -> ValList V.empty

valueToInt :: MonadError Error m => Value -> m Integer
valueToInt = \case
  ValInt n -> return n
  val -> throwRuntimeError $ "Internal Error: not int: " ++ show val

valueToIntList :: MonadError Error m => V.Vector Value -> m [Integer]
valueToIntList = mapM valueToInt . V.toList

valueToIntList' :: MonadError Error m => Value -> m [Integer]
valueToIntList' (ValList xs) = valueToIntList xs
valueToIntList' _ = throwRuntimeError "Internal Error: type error"

valueToBool :: MonadError Error m => Value -> m Bool
valueToBool = \case
  ValBool p -> return p
  val -> throwRuntimeError $ "Internal Error: not bool: " ++ show val

valueToBoolList :: MonadError Error m => V.Vector Value -> m [Bool]
valueToBoolList = mapM valueToBool . V.toList

valueToVector :: MonadError Error m => Value -> m (V.Vector Integer)
valueToVector (ValTuple x) = V.fromList <$> mapM valueToInt x
valueToVector _ = throwRuntimeError "Internal Error: value is not a vector"

valueToMatrix :: MonadError Error m => Value -> m (Matrix Integer)
valueToMatrix (ValTuple f) = do
  f <- V.fromList <$> mapM valueToVector f
  case makeMatrix f of
    Nothing -> throwRuntimeError "Internal Error: value is not a matrix"
    Just f -> return f
valueToMatrix _ = throwRuntimeError "Internal Error: value is not a matrix"

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

formatValue :: Value -> String
formatValue = \case
  ValInt n -> show n
  ValBool p -> map toLower (show p)
  ValList xs -> "[" ++ intercalate ", " (map formatValue (V.toList xs)) ++ "]"
  ValTuple [x] -> "(" ++ formatValue x ++ ",)"
  ValTuple xs -> "(" ++ intercalate ", " (map formatValue xs) ++ ")"
  ValBuiltin builtin -> show builtin
  f@ValLambda {} -> show f

writeValueIO :: Value -> IO ()
writeValueIO = \case
  ValInt n -> print n
  ValBool p -> putStrLn (if p then "Yes" else "No")
  ValList xs -> do
    print (V.length xs)
    mapM_ writeValueIO xs
  ValTuple xs -> mapM_ writeValueIO xs
  ValBuiltin builtin -> print builtin
  f@ValLambda {} -> print f
