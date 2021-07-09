{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Language.Util where

import Control.Monad.Identity
import Data.Maybe (isJust)
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
  -- advanced arithmetical functions
  Abs -> Abs
  Gcd -> Gcd
  Lcm -> Lcm
  Min2 t -> Min2 (f t)
  Max2 t -> Max2 (f t)
  Iterate t -> Iterate (f t)
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
  VecFloorMod n -> VecFloorMod n
  MatFloorMod h w -> MatFloorMod h w
  -- modular functionsmodular
  ModNegate -> ModNegate
  ModPlus -> ModPlus
  ModMinus -> ModMinus
  ModMult -> ModMult
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
  Map t1 t2 -> Map (f t1) (f t2)
  Filter t -> Filter (f t)
  At t -> At (f t)
  SetAt t -> SetAt (f t)
  Elem t -> Elem (f t)
  Sum -> Sum
  Product -> Product
  ModSum -> ModSum
  ModProduct -> ModProduct
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
  App f e -> countOccurrences x f + countOccurrences x e
  Lam y _ body -> if x == y then 0 else countOccurrences x body
  Let y _ e1 e2 -> countOccurrences x e1 + (if x == y then 0 else countOccurrences x e2)

countOccurrencesToplevelExpr :: VarName -> ToplevelExpr -> Int
countOccurrencesToplevelExpr x = \case
  ResultExpr e -> countOccurrences x e
  ToplevelLet y _ e cont -> countOccurrences x e + (if x == y then 0 else countOccurrencesToplevelExpr x cont)
  ToplevelLetRec f args _ body cont -> if x == f then 0 else countOccurrencesToplevelExpr x cont + (if x `elem` map fst args then 0 else countOccurrences x body)

-- | `mapExprM'` substitutes exprs using given two functions, which are called in pre-order and post-order.
mapExprM' :: Monad m => ([(VarName, Type)] -> Expr -> m Expr) -> ([(VarName, Type)] -> Expr -> m Expr) -> [(VarName, Type)] -> Expr -> m Expr
mapExprM' pre post env e = do
  e <- pre env e
  let go = mapExprM' pre post
  e <- case e of
    Var y -> return $ Var y
    Lit lit -> return $ Lit lit
    App g e -> App <$> go env g <*> go env e
    Lam x t body -> Lam x t <$> go ((x, t) : env) body
    Let y t e1 e2 -> Let y t <$> go env e1 <*> go ((y, t) : env) e2
  post env e

mapExprToplevelExprM' :: Monad m => ([(VarName, Type)] -> Expr -> m Expr) -> ([(VarName, Type)] -> Expr -> m Expr) -> [(VarName, Type)] -> ToplevelExpr -> m ToplevelExpr
mapExprToplevelExprM' pre post env = \case
  ResultExpr e -> ResultExpr <$> mapExprM' pre post env e
  ToplevelLet y t e cont ->
    let env' = (y, t) : env
     in ToplevelLet y t <$> mapExprM' pre post env' e <*> mapExprToplevelExprM' pre post env' cont
  ToplevelLetRec g args ret body cont ->
    let env' = (g, foldr (FunTy . snd) ret args) : env
     in ToplevelLetRec g args ret <$> mapExprM' pre post (reverse args ++ env) body <*> mapExprToplevelExprM' pre post env' cont

mapExprProgramM' :: Monad m => ([(VarName, Type)] -> Expr -> m Expr) -> ([(VarName, Type)] -> Expr -> m Expr) -> Program -> m Program
mapExprProgramM' pre post = mapExprToplevelExprM' pre post []

-- | `mapExprM` is a wrapper of `mapExprM'`. This function works in post-order.
mapExprM :: Monad m => ([(VarName, Type)] -> Expr -> m Expr) -> [(VarName, Type)] -> Expr -> m Expr
mapExprM f = mapExprM' (\_ e -> return e) f

mapExprToplevelExprM :: Monad m => ([(VarName, Type)] -> Expr -> m Expr) -> [(VarName, Type)] -> ToplevelExpr -> m ToplevelExpr
mapExprToplevelExprM f = mapExprToplevelExprM' (\_ e -> return e) f

mapExprProgramM :: Monad m => ([(VarName, Type)] -> Expr -> m Expr) -> Program -> m Program
mapExprProgramM f = mapExprProgramM' (\_ e -> return e) f

mapExpr :: ([(VarName, Type)] -> Expr -> Expr) -> [(VarName, Type)] -> Expr -> Expr
mapExpr f env e = runIdentity $ mapExprM (\env e -> return $ f env e) env e

mapExprToplevelExpr :: ([(VarName, Type)] -> Expr -> Expr) -> [(VarName, Type)] -> ToplevelExpr -> ToplevelExpr
mapExprToplevelExpr f env e = runIdentity $ mapExprToplevelExprM (\env e -> return $ f env e) env e

mapExprProgram :: ([(VarName, Type)] -> Expr -> Expr) -> Program -> Program
mapExprProgram f prog = runIdentity $ mapExprProgramM (\env e -> return $ f env e) prog

uncurryFunTy :: Type -> ([Type], Type)
uncurryFunTy = \case
  (FunTy t t') -> let (ts, ret) = uncurryFunTy t' in (t : ts, ret)
  ret -> ([], ret)

uncurryLam :: Expr -> ([(VarName, Type)], Expr)
uncurryLam = \case
  Lam x t body -> let (args, body') = uncurryLam body in ((x, t) : args, body')
  body -> ([], body)

curryApp :: Expr -> (Expr, [Expr])
curryApp = \case
  App f e -> let (f', e') = curryApp f in (f', e' ++ [e])
  f -> (f, [])

curryFunTy :: [Type] -> Type -> Type
curryFunTy ts ret = foldr FunTy ret ts

curryLam :: [(VarName, Type)] -> Expr -> Expr
curryLam args body = foldr (uncurry Lam) body args

uncurryApp :: Expr -> [Expr] -> Expr
uncurryApp = foldl App

isVectorTy :: Type -> Bool
isVectorTy = isJust . sizeOfVectorTy

isVectorTy' :: [Type] -> Bool
isVectorTy' = isVectorTy . TupleTy

sizeOfVectorTy :: Type -> Maybe Int
sizeOfVectorTy = \case
  TupleTy ts | all (== IntTy) ts -> Just (length ts)
  _ -> Nothing

isMatrixTy :: Type -> Bool
isMatrixTy = isJust . sizeOfMatrixTy

isMatrixTy' :: [Type] -> Bool
isMatrixTy' = isMatrixTy . TupleTy

sizeOfMatrixTy :: Type -> Maybe (Int, Int)
sizeOfMatrixTy = \case
  TupleTy ts@(TupleTy ts' : _) | all (== IntTy) ts' && all (== TupleTy ts') ts -> Just (length ts, length ts')
  _ -> Nothing
