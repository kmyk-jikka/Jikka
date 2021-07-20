{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Jikka.RestrictedPython.Language.Builtin where

import Data.Functor
import qualified Data.Set as S
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util

builtinNames :: S.Set VarName
builtinNames = S.union standardBuiltinNames additionalBuiltinNames

standardBuiltinNames :: S.Set VarName
standardBuiltinNames =
  S.fromList
    [ "abs",
      "all",
      "any",
      "bool",
      "divmod",
      "enumerate",
      "filter",
      "input",
      "int",
      "len",
      "list",
      "map",
      "max",
      "min",
      "pow",
      "print",
      "range",
      "reversed",
      "sorted",
      "sum",
      "zip"
    ]

additionalBuiltinNames :: S.Set VarName
additionalBuiltinNames =
  S.fromList
    [ "argmax",
      "argmin",
      "ceildiv",
      "ceilmod",
      "choose",
      "fact",
      "floordiv",
      "floormod",
      "gcd",
      "inv",
      "lcm",
      "multichoose",
      "permute",
      "product"
    ]

-- | `resolveUniqueBuiltin` makes a builtin function from a variable name.
-- However, this doesn't anything for ambiguous builtin functions.
-- For example, the builtin function "max" is kept as a variable because it may be \(\mathbf{list}(\alpha) \to \alpha\), \(\alpha \times \alpha \to \alpha\), etc. and this function cannot resolve it.
resolveUniqueBuiltin :: (MonadAlpha m, MonadError Error m) => VarName' -> m Expr'
resolveUniqueBuiltin x | value' x `S.notMember` builtinNames = return $ WithLoc' (loc' x) (Name x)
resolveUniqueBuiltin x = do
  let f = return . WithLoc' (loc' x) . Constant . ConstBuiltin
  case value' x of
    "abs" -> f BuiltinAbs
    "all" -> f BuiltinAll
    "any" -> f BuiltinAny
    "bool" -> f . BuiltinBool =<< genType
    "divmod" -> f BuiltinDivMod
    "enumerate" -> f . BuiltinEnumerate =<< genType
    "filter" -> f . BuiltinFilter =<< genType
    "int" -> f . BuiltinInt =<< genType
    "input" -> f BuiltinInput
    "len" -> f . BuiltinLen =<< genType
    "list" -> f . BuiltinList =<< genType
    "reversed" -> f . BuiltinReversed =<< genType
    "sorted" -> f . BuiltinSorted =<< genType
    "sum" -> f BuiltinSum
    "argmax" -> f . BuiltinArgMax =<< genType
    "argmin" -> f . BuiltinArgMin =<< genType
    "ceildiv" -> f BuiltinCeilDiv
    "ceilmod" -> f BuiltinCeilMod
    "choose" -> f BuiltinChoose
    "fact" -> f BuiltinFact
    "floordiv" -> f BuiltinFloorDiv
    "floormod" -> f BuiltinFloorMod
    "gcd" -> f BuiltinGcd
    "inv" -> f BuiltinModInv
    "lcm" -> f BuiltinLcm
    "multichoose" -> f BuiltinMultiChoose
    "permute" -> f BuiltinPermute
    "product" -> f BuiltinProduct
    _ -> return $ WithLoc' (loc' x) (Name x)

resolveBuiltin :: (MonadAlpha m, MonadError Error m) => VarName' -> Int -> m Expr'
resolveBuiltin x _ | value' x `S.notMember` builtinNames = return $ WithLoc' (loc' x) (Name x)
resolveBuiltin x n = wrapAt' (loc' x) . wrapError' "Jikka.RestrictedPython.Language.Builtin.resolveBuiltin" $ do
  let f = return . WithLoc' (loc' x) . Constant . ConstBuiltin
  when (n < 0) $ do
    throwInternalError $ "negative arity: " ++ show n
  case value' x of
    "map" -> f =<< (BuiltinMap <$> replicateM (n - 1) genType <*> genType)
    "max" -> case n of
      1 -> f . BuiltinMax1 =<< genType
      _ -> f =<< (BuiltinMax <$> genType <*> pure n)
    "min" -> case n of
      1 -> f . BuiltinMin1 =<< genType
      _ -> f =<< (BuiltinMin <$> genType <*> pure n)
    "pow" ->
      if n == 3
        then f BuiltinModPow
        else f BuiltinPow
    "print" -> f . BuiltinPrint =<< replicateM n genType
    "range" -> case n of
      1 -> f BuiltinRange1
      2 -> f BuiltinRange2
      3 -> f BuiltinRange3
      _ -> throwTypeError $ "range expected 1, 2, or 3 arguments, got " ++ show n
    "zip" -> f . BuiltinZip =<< replicateM n genType
    _ -> do
      e <- resolveUniqueBuiltin x
      case value' e of
        Constant (ConstBuiltin _) -> return e
        _ -> throwInternalError $ "not exhaustive: " ++ unVarName (value' x)

formatBuiltin :: Builtin -> String
formatBuiltin = \case
  BuiltinAbs -> "abs"
  BuiltinPow -> "pow"
  BuiltinModPow -> "pow"
  BuiltinAll -> "all"
  BuiltinAny -> "any"
  BuiltinDivMod -> "divmod"
  BuiltinSorted _ -> "sorted"
  BuiltinEnumerate _ -> "enumerate"
  BuiltinBool _ -> "bool"
  BuiltinInt _ -> "int"
  BuiltinSum -> "sum"
  BuiltinZip _ -> "zip"
  BuiltinFilter _ -> "filter"
  BuiltinTuple _ -> "tuple"
  BuiltinLen _ -> "len"
  BuiltinList _ -> "list"
  BuiltinRange1 -> "range"
  BuiltinRange2 -> "range"
  BuiltinRange3 -> "range"
  BuiltinMap _ _ -> "map"
  BuiltinReversed _ -> "reversed"
  BuiltinMax1 _ -> "max"
  BuiltinMax _ _ -> "max"
  BuiltinMin1 _ -> "min"
  BuiltinMin _ _ -> "min"
  BuiltinArgMax _ -> "argmax"
  BuiltinArgMin _ -> "argmin"
  BuiltinCeilDiv -> "ceildiv"
  BuiltinCeilMod -> "ceilmod"
  BuiltinFloorDiv -> "floordiv"
  BuiltinFloorMod -> "floormod"
  BuiltinChoose -> "choose"
  BuiltinFact -> "fact"
  BuiltinGcd -> "gcd"
  BuiltinLcm -> "lcm"
  BuiltinModInv -> "inv"
  BuiltinMultiChoose -> "multichoose"
  BuiltinPermute -> "permute"
  BuiltinProduct -> "product"
  BuiltinInput -> "input"
  BuiltinPrint _ -> "print"

typeBuiltin :: Builtin -> Type
typeBuiltin = \case
  BuiltinAbs -> CallableTy [IntTy] IntTy
  BuiltinPow -> CallableTy [IntTy] IntTy
  BuiltinModPow -> CallableTy [IntTy, IntTy] IntTy
  BuiltinAll -> CallableTy [ListTy BoolTy] BoolTy
  BuiltinAny -> CallableTy [ListTy BoolTy] BoolTy
  BuiltinArgMax t -> CallableTy [ListTy t] IntTy
  BuiltinArgMin t -> CallableTy [ListTy t] IntTy
  BuiltinBool t -> CallableTy [t] BoolTy
  BuiltinCeilDiv -> CallableTy [IntTy, IntTy] IntTy
  BuiltinCeilMod -> CallableTy [IntTy, IntTy] IntTy
  BuiltinChoose -> CallableTy [IntTy, IntTy] IntTy
  BuiltinDivMod -> CallableTy [IntTy, IntTy] (TupleTy [IntTy, IntTy])
  BuiltinEnumerate t -> CallableTy [ListTy t] (ListTy (TupleTy [IntTy, t]))
  BuiltinFact -> CallableTy [ListTy IntTy] IntTy
  BuiltinFilter t -> CallableTy [CallableTy [t] BoolTy, ListTy t] (ListTy t)
  BuiltinFloorDiv -> CallableTy [IntTy, IntTy] IntTy
  BuiltinFloorMod -> CallableTy [IntTy, IntTy] IntTy
  BuiltinGcd -> CallableTy [IntTy, IntTy] IntTy
  BuiltinInt t -> CallableTy [t] IntTy
  BuiltinModInv -> CallableTy [IntTy, IntTy] IntTy
  BuiltinLcm -> CallableTy [IntTy, IntTy] IntTy
  BuiltinLen t -> CallableTy [ListTy t] IntTy
  BuiltinList t -> CallableTy [ListTy t] (ListTy t)
  BuiltinMap args ret -> CallableTy (CallableTy args ret : map ListTy args) (ListTy ret)
  BuiltinMax t n -> CallableTy (replicate n t) t
  BuiltinMax1 t -> CallableTy [ListTy t] t
  BuiltinMin t n -> CallableTy (replicate n t) t
  BuiltinMin1 t -> CallableTy [ListTy t] t
  BuiltinMultiChoose -> CallableTy [IntTy, IntTy] IntTy
  BuiltinPermute -> CallableTy [IntTy, IntTy] IntTy
  BuiltinProduct -> CallableTy [ListTy IntTy] IntTy
  BuiltinRange1 -> CallableTy [IntTy] (ListTy IntTy)
  BuiltinRange2 -> CallableTy [IntTy, IntTy] (ListTy IntTy)
  BuiltinRange3 -> CallableTy [IntTy, IntTy, IntTy] (ListTy IntTy)
  BuiltinReversed t -> CallableTy [ListTy t] (ListTy t)
  BuiltinSorted t -> CallableTy [ListTy t] (ListTy t)
  BuiltinSum -> CallableTy [ListTy IntTy] IntTy
  BuiltinTuple ts -> CallableTy [TupleTy ts] (TupleTy ts)
  BuiltinZip ts -> CallableTy (map ListTy ts) (TupleTy ts)
  BuiltinInput -> CallableTy [] StringTy
  BuiltinPrint ts -> CallableTy ts SideEffectTy

mapTypeBuiltin :: (Type -> Type) -> Builtin -> Builtin
mapTypeBuiltin f = \case
  BuiltinAbs -> BuiltinAbs
  BuiltinPow -> BuiltinPow
  BuiltinModPow -> BuiltinModPow
  BuiltinAll -> BuiltinAll
  BuiltinAny -> BuiltinAny
  BuiltinArgMax t -> BuiltinArgMax (f t)
  BuiltinArgMin t -> BuiltinArgMin (f t)
  BuiltinBool t -> BuiltinBool (f t)
  BuiltinCeilDiv -> BuiltinCeilDiv
  BuiltinCeilMod -> BuiltinCeilMod
  BuiltinChoose -> BuiltinChoose
  BuiltinDivMod -> BuiltinDivMod
  BuiltinEnumerate t -> BuiltinEnumerate (f t)
  BuiltinFact -> BuiltinFact
  BuiltinFilter t -> BuiltinFilter (f t)
  BuiltinFloorDiv -> BuiltinFloorDiv
  BuiltinFloorMod -> BuiltinFloorMod
  BuiltinGcd -> BuiltinGcd
  BuiltinInt t -> BuiltinInt (f t)
  BuiltinModInv -> BuiltinModInv
  BuiltinLcm -> BuiltinLcm
  BuiltinLen t -> BuiltinLen (f t)
  BuiltinList t -> BuiltinList (f t)
  BuiltinMap args ret -> BuiltinMap (map f args) (f ret)
  BuiltinMax t n -> BuiltinMax (f t) n
  BuiltinMax1 t -> BuiltinMax1 (f t)
  BuiltinMin t n -> BuiltinMin (f t) n
  BuiltinMin1 t -> BuiltinMin1 (f t)
  BuiltinMultiChoose -> BuiltinMultiChoose
  BuiltinPermute -> BuiltinPermute
  BuiltinProduct -> BuiltinProduct
  BuiltinRange1 -> BuiltinRange1
  BuiltinRange2 -> BuiltinRange2
  BuiltinRange3 -> BuiltinRange3
  BuiltinReversed t -> BuiltinReversed (f t)
  BuiltinSorted t -> BuiltinSorted (f t)
  BuiltinSum -> BuiltinSum
  BuiltinTuple ts -> BuiltinTuple (map f ts)
  BuiltinZip ts -> BuiltinZip (map f ts)
  BuiltinInput -> BuiltinInput
  BuiltinPrint ts -> BuiltinPrint (map f ts)

attributeNames :: S.Set AttributeName
attributeNames =
  S.fromList
    [ "count",
      "index",
      "copy",
      "append",
      "split"
    ]

resolveAttribute' :: (MonadAlpha m, MonadError Error m) => Attribute' -> m Attribute'
resolveAttribute' x = wrapAt' (loc' x) $ case value' x of
  UnresolvedAttribute x' ->
    if x' `S.notMember` attributeNames
      then throwSymbolError $ "unknown attribute: " ++ unAttributeName x'
      else wrapError' "Jikka.RestrictedPython.Language.Builtin.resolveAttribute" $ do
        WithLoc' (loc' x) <$> case x' of
          "count" -> BuiltinCount <$> genType
          "index" -> BuiltinIndex <$> genType
          "copy" -> BuiltinCopy <$> genType
          "append" -> BuiltinAppend <$> genType
          "split" -> return BuiltinSplit
          _ -> throwInternalError $ "not exhaustive: " ++ unAttributeName x'
  _ -> return x

resolveAttribute :: (MonadAlpha m, MonadError Error m) => Expr' -> Attribute' -> m Expr
resolveAttribute e@(WithLoc' _ (Name (WithLoc' _ "math"))) x = wrapAt' (loc' x) $ case value' x of
  UnresolvedAttribute x' -> case x' of
    "gcd" -> return (Constant (ConstBuiltin BuiltinGcd))
    "lcm" -> return (Constant (ConstBuiltin BuiltinGcd))
    _ -> throwSymbolError $ "unknown attribute: " ++ unAttributeName x'
  _ -> return $ Attribute e x
resolveAttribute e@(WithLoc' _ (Name (WithLoc' _ "jikka"))) x = wrapAt' (loc' x) $ case value' x of
  UnresolvedAttribute x' ->
    let x'' = VarName (unAttributeName x')
     in if x'' `S.notMember` additionalBuiltinNames
          then throwSymbolError $ "unknown attribute: " ++ unAttributeName x'
          else value' <$> resolveUniqueBuiltin (x $> x'')
  _ -> return $ Attribute e x
resolveAttribute e x = Attribute e <$> resolveAttribute' x

formatAttribute :: Attribute -> String
formatAttribute = \case
  UnresolvedAttribute x -> unAttributeName x
  BuiltinCount _ -> "count"
  BuiltinIndex _ -> "index"
  BuiltinCopy _ -> "copy"
  BuiltinAppend _ -> "append"
  BuiltinSplit -> "split"

typeAttribute :: Attribute -> (Type, Type)
typeAttribute = \case
  UnresolvedAttribute x -> error $ "Jikka.RestrictedPython.Language.Builtin.typeAttribute: attributes must be resolved: " ++ unAttributeName x
  BuiltinCount t -> (ListTy t, CallableTy [t] IntTy)
  BuiltinIndex t -> (ListTy t, CallableTy [t] IntTy)
  BuiltinCopy t -> (ListTy t, CallableTy [] (ListTy t))
  BuiltinAppend t -> (ListTy t, CallableTy [t] SideEffectTy)
  BuiltinSplit -> (StringTy, CallableTy [] (ListTy StringTy))

mapTypeAttribute :: (Type -> Type) -> Attribute -> Attribute
mapTypeAttribute f = \case
  UnresolvedAttribute x -> UnresolvedAttribute x
  BuiltinCount t -> BuiltinCount (f t)
  BuiltinIndex t -> BuiltinIndex (f t)
  BuiltinCopy t -> BuiltinCopy (f t)
  BuiltinAppend t -> BuiltinAppend (f t)
  BuiltinSplit -> BuiltinSplit
