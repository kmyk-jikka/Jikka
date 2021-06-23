{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Language.Util where

import Jikka.Common.Alpha
import Jikka.Core.Language.Expr

genType :: MonadAlpha m => m Type
genType = do
  i <- nextCounter
  return $ VarTy (TypeName ('$' : show i))

genVarName :: MonadAlpha m => VarName -> m VarName
genVarName x = do
  i <- nextCounter
  let base = if unVarName x == "_" then "" else takeWhile (/= '$') (unVarName x)
  return $ VarName (base ++ '$' : show i)

genVarName' :: MonadAlpha m => m VarName
genVarName' = genVarName (VarName "_")

mapTypeInBuiltin :: (Type -> Type) -> Builtin -> Builtin
mapTypeInBuiltin f = \case
  -- arithmetical functions
  Negate -> Negate
  Plus -> Plus
  Minus -> Minus
  Mult -> Mult
  FloorDiv -> FloorDiv
  FloorMod -> FloorMod
  CeilDiv -> CeilDiv
  CeilMod -> CeilMod
  Pow -> Pow
  -- induction functions
  NatInd t -> NatInd (f t)
  -- advanced arithmetical functions
  Abs -> Abs
  Gcd -> Gcd
  Lcm -> Lcm
  Min2 t -> Min2 (f t)
  Max2 t -> Max2 (f t)
  -- logical functionslogical
  Not -> Not
  And -> And
  Or -> Or
  Implies -> Implies
  If t -> If (f t)
  -- bitwise functionsbitwise
  BitNot -> BitNot
  BitAnd -> BitAnd
  BitOr -> BitOr
  BitXor -> BitXor
  BitLeftShift -> BitLeftShift
  BitRightShift -> BitRightShift
  -- matrix functions
  MatAp h w -> MatAp h w
  MatZero n -> MatZero n
  MatOne n -> MatOne n
  MatAdd h w -> MatAdd h w
  MatMul h n w -> MatMul h n w
  MatPow n -> MatPow n
  -- modular functionsmodular
  ModInv -> ModInv
  ModPow -> ModPow
  ModMatAp h w -> ModMatAp h w
  ModMatAdd h w -> ModMatAdd h w
  ModMatMul h n w -> ModMatMul h n w
  ModMatPow n -> ModMatPow n
  -- list functionslist
  Cons t -> Cons (f t)
  Foldl t1 t2 -> Foldl (f t1) (f t2)
  Scanl t1 t2 -> Scanl (f t1) (f t2)
  Len t -> Len (f t)
  Tabulate t -> Tabulate (f t)
  Map t1 t2 -> Map (f t1) (f t2)
  Filter t -> Filter (f t)
  At t -> At (f t)
  SetAt t -> SetAt (f t)
  Elem t -> Elem (f t)
  Sum -> Sum
  Product -> Product
  Min1 t -> Min1 (f t)
  Max1 t -> Max1 (f t)
  ArgMin t -> ArgMin (f t)
  ArgMax t -> ArgMax (f t)
  All -> All
  Any -> Any
  Sorted t -> Sorted (f t)
  List t -> List (f t)
  Reversed t -> Reversed (f t)
  Range1 -> Range1
  Range2 -> Range2
  Range3 -> Range3
  -- tuple functions
  Tuple ts -> Tuple (map f ts)
  Proj ts n -> Proj (map f ts) n
  -- comparison
  LessThan t -> LessThan (f t)
  LessEqual t -> LessEqual (f t)
  GreaterThan t -> GreaterThan (f t)
  GreaterEqual t -> GreaterEqual (f t)
  Equal t -> Equal (f t)
  NotEqual t -> NotEqual (f t)
  -- combinational functions
  Fact -> Fact
  Choose -> Choose
  Permute -> Permute
  MultiChoose -> MultiChoose

countOccurrences :: VarName -> Expr -> Int
countOccurrences x = \case
  Var y -> if x == y then 1 else 0
  Lit _ -> 0
  App f args -> sum (map (countOccurrences x) (f : args))
  Lam args body -> if x `elem` map fst args then 0 else countOccurrences x body
  Let y _ e1 e2 -> countOccurrences x e1 + (if x == y then 0 else countOccurrences x e2)

countOccurrencesToplevelExpr :: VarName -> ToplevelExpr -> Int
countOccurrencesToplevelExpr x = \case
  ResultExpr e -> countOccurrences x e
  ToplevelLet y _ e cont -> countOccurrences x e + (if x == y then 0 else countOccurrencesToplevelExpr x cont)
  ToplevelLetRec f args _ body cont -> if x == f then 0 else countOccurrencesToplevelExpr x cont + (if x `elem` map fst args then 0 else countOccurrences x body)
