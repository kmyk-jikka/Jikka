{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Util where

import Data.List (delete)
import Jikka.Common.Alpha
import Jikka.RestrictedPython.Language.Expr

genType :: MonadAlpha m => m Type
genType = do
  i <- nextCounter
  return $ VarTy (TypeName ('$' : show i))

freeTyVars :: Type -> [TypeName]
freeTyVars = \case
  VarTy x -> [x]
  IntTy -> []
  BoolTy -> []
  ListTy t -> freeTyVars t
  TupleTy ts -> concat $ mapM freeTyVars ts
  CallableTy ts ret -> concat $ mapM freeTyVars (ret : ts)

freeVars :: Expr -> [VarName]
freeVars = \case
  BoolOp e1 _ e2 -> freeVars e1 ++ freeVars e2
  BinOp e1 _ e2 -> freeVars e1 ++ freeVars e2
  UnaryOp _ e -> freeVars e
  Lambda args e -> foldl (\vars (x, _) -> delete x vars) (freeVars e) args
  IfExp e1 e2 e3 -> freeVars e1 ++ freeVars e2 ++ freeVars e3
  ListComp e (Comprehension x iter pred) -> freeVars iter ++ foldl (\vars x -> delete x vars) (freeVars e ++ concatMap freeVars pred) (targetVars x)
  Compare e1 _ e2 -> freeVars e1 ++ freeVars e2
  Call f args -> concatMap freeVars (f : args)
  Constant _ -> []
  Subscript e1 e2 -> freeVars e1 ++ freeVars e2
  Name x -> [x]
  List _ es -> concatMap freeVars es
  Tuple es -> concatMap freeVars es
  SubscriptSlice e from to step -> freeVars e ++ concatMap freeVars from ++ concatMap freeVars to ++ concatMap freeVars step

targetVars :: Target -> [VarName]
targetVars = \case
  SubscriptTrg x _ -> targetVars x
  NameTrg x -> [x]
  TupleTrg xs -> concatMap targetVars xs
