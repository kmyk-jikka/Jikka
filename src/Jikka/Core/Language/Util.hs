{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Language.Util where

import Control.Monad.Identity
import Control.Monad.Writer (execWriter, tell)
import Data.Maybe (isJust)
import Data.Monoid (Dual (..))
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
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
  Snoc t -> Snoc (f t)
  Foldl t1 t2 -> Foldl (f t1) (f t2)
  Scanl t1 t2 -> Scanl (f t1) (f t2)
  Build t -> Build (f t)
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
  -- data structures
  ConvexHullTrickInit -> ConvexHullTrickInit
  ConvexHullTrickInsert -> ConvexHullTrickInsert
  ConvexHullTrickGetMin -> ConvexHullTrickGetMin
  SegmentTreeInitList semigrp -> SegmentTreeInitList semigrp
  SegmentTreeGetRange semigrp -> SegmentTreeGetRange semigrp
  SegmentTreeSetPoint semigrp -> SegmentTreeSetPoint semigrp

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
    ToplevelLet y t <$> mapExprM' pre post env e <*> mapExprToplevelExprM' pre post ((y, t) : env) cont
  ToplevelLetRec g args ret body cont ->
    let env' = (g, foldr (FunTy . snd) ret args) : env
     in ToplevelLetRec g args ret <$> mapExprM' pre post (reverse args ++ env') body <*> mapExprToplevelExprM' pre post env' cont

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

listSubExprs :: Expr -> [Expr]
listSubExprs e = getDual . execWriter $ mapExprM go [] e
  where
    go _ e = do
      tell $ Dual [e]
      return e

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

isConstantTimeBuiltin :: Builtin -> Bool
isConstantTimeBuiltin = \case
  -- arithmetical functions
  Negate -> True
  Plus -> True
  Minus -> True
  Mult -> True
  FloorDiv -> True
  FloorMod -> True
  CeilDiv -> True
  CeilMod -> True
  Pow -> True
  -- advanced arithmetical functions
  Abs -> True
  Gcd -> True
  Lcm -> True
  Min2 _ -> True
  Max2 _ -> True
  Iterate _ -> False
  -- logical functions
  Not -> True
  And -> True
  Or -> True
  Implies -> True
  If _ -> True
  -- bitwise functions
  BitNot -> True
  BitAnd -> True
  BitOr -> True
  BitXor -> True
  BitLeftShift -> True
  BitRightShift -> True
  -- matrix functions
  MatAp _ _ -> True
  MatZero _ -> True
  MatOne _ -> True
  MatAdd _ _ -> True
  MatMul _ _ _ -> True
  MatPow _ -> True
  VecFloorMod _ -> True
  MatFloorMod _ _ -> True
  -- modular functions
  ModNegate -> True
  ModPlus -> True
  ModMinus -> True
  ModMult -> True
  ModInv -> True
  ModPow -> True
  ModMatAp _ _ -> True
  ModMatAdd _ _ -> True
  ModMatMul _ _ _ -> True
  ModMatPow _ -> True
  -- list functions
  Cons _ -> False
  Snoc _ -> False
  Foldl _ _ -> False
  Scanl _ _ -> False
  Build _ -> False
  Len _ -> True
  Map _ _ -> False
  Filter _ -> False
  At _ -> True
  SetAt _ -> False
  Elem _ -> False
  Sum -> False
  Product -> False
  ModSum -> False
  ModProduct -> False
  Min1 _ -> False
  Max1 _ -> False
  ArgMin _ -> False
  ArgMax _ -> False
  All -> False
  Any -> False
  Sorted _ -> False
  Reversed _ -> False
  Range1 -> False
  Range2 -> False
  Range3 -> False
  -- tuple functions
  Tuple _ -> True
  Proj _ _ -> True
  -- comparison
  LessThan _ -> True
  LessEqual _ -> True
  GreaterThan _ -> True
  GreaterEqual _ -> True
  Equal _ -> True
  NotEqual _ -> True
  -- combinational functions
  Fact -> True
  Choose -> True
  Permute -> True
  MultiChoose -> True
  -- data structures
  ConvexHullTrickInit -> False
  ConvexHullTrickInsert -> False
  ConvexHullTrickGetMin -> False
  SegmentTreeInitList _ -> False
  SegmentTreeGetRange _ -> False
  SegmentTreeSetPoint _ -> False

-- | `isConstantTimeExpr` checks whether given exprs are suitable to propagate.
isConstantTimeExpr :: Expr -> Bool
isConstantTimeExpr = \case
  Var _ -> True
  Lit _ -> True
  e@(App _ _) -> case curryApp e of
    (Lit (LitBuiltin f), args) -> isConstantTimeBuiltin f && all isConstantTimeExpr args
    _ -> False
  Lam _ _ _ -> True
  Let _ _ e1 e2 -> isConstantTimeExpr e1 && isConstantTimeExpr e2

-- | `replaceLenF` replaces @len(f)@ in an expr with @i + 1@.
-- * This assumes that there are no name conflicts.
replaceLenF :: MonadError Error m => VarName -> VarName -> Expr -> m Expr
replaceLenF f i = go
  where
    go = \case
      Len' _ (Var f') | f' == f -> return $ Plus' (Var i) (LitInt' 1)
      Var y -> return $ Var y
      Lit lit -> return $ Lit lit
      App g e -> App <$> go g <*> go e
      Lam x _ _ | x == i -> throwInternalError "Jikka.Core.Language.Util.replaceLenF: name conflict"
      Lam x t body -> Lam x t <$> (if x == f then return body else go body)
      Let y _ _ _ | y == i -> throwInternalError "Jikka.Core.Language.Util.replaceLenF: name conflict"
      Let y t e1 e2 -> Let y t <$> go e1 <*> (if y == f then return e2 else go e2)
