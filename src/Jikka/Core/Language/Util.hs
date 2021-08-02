{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Jikka.Core.Language.Util where

import Control.Arrow
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Writer (execWriter, tell)
import Data.Maybe
import Data.Monoid (Dual (..))
import qualified Data.Vector as V
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

mapSubTypesM :: Monad m => (Type -> m Type) -> Type -> m Type
mapSubTypesM f = go
  where
    go = \case
      VarTy x -> f $ VarTy x
      IntTy -> f IntTy
      BoolTy -> f BoolTy
      ListTy t -> f . ListTy =<< f t
      TupleTy ts -> f . TupleTy =<< mapM f ts
      FunTy t1 t2 -> f =<< (FunTy <$> f t1 <*> f t2)
      DataStructureTy ds -> f $ DataStructureTy ds

mapTypeBuiltinM :: Monad m => (Type -> m Type) -> Builtin -> m Builtin
mapTypeBuiltinM f = \case
  -- arithmetical functions
  Negate -> return Negate
  Plus -> return Plus
  Minus -> return Minus
  Mult -> return Mult
  FloorDiv -> return FloorDiv
  FloorMod -> return FloorMod
  CeilDiv -> return CeilDiv
  CeilMod -> return CeilMod
  Pow -> return Pow
  -- advanced arithmetical functions
  Abs -> return Abs
  Gcd -> return Gcd
  Lcm -> return Lcm
  Min2 t -> Min2 <$> f t
  Max2 t -> Max2 <$> f t
  Iterate t -> Iterate <$> f t
  -- logical functionslogical
  Not -> return Not
  And -> return And
  Or -> return Or
  Implies -> return Implies
  If t -> If <$> f t
  -- bitwise functionsbitwise
  BitNot -> return BitNot
  BitAnd -> return BitAnd
  BitOr -> return BitOr
  BitXor -> return BitXor
  BitLeftShift -> return BitLeftShift
  BitRightShift -> return BitRightShift
  -- matrix functions
  MatAp h w -> return $ MatAp h w
  MatZero n -> return $ MatZero n
  MatOne n -> return $ MatOne n
  MatAdd h w -> return $ MatAdd h w
  MatMul h n w -> return $ MatMul h n w
  MatPow n -> return $ MatPow n
  VecFloorMod n -> return $ VecFloorMod n
  MatFloorMod h w -> return $ MatFloorMod h w
  -- modular functionsmodular
  ModNegate -> return ModNegate
  ModPlus -> return ModPlus
  ModMinus -> return ModMinus
  ModMult -> return ModMult
  ModInv -> return ModInv
  ModPow -> return ModPow
  ModMatAp h w -> return $ ModMatAp h w
  ModMatAdd h w -> return $ ModMatAdd h w
  ModMatMul h n w -> return $ ModMatMul h n w
  ModMatPow n -> return $ ModMatPow n
  -- list functionslist
  Cons t -> Cons <$> f t
  Snoc t -> Snoc <$> f t
  Foldl t1 t2 -> Foldl <$> f t1 <*> f t2
  Scanl t1 t2 -> Scanl <$> f t1 <*> f t2
  Build t -> Build <$> f t
  Len t -> Len <$> f t
  Map t1 t2 -> Map <$> f t1 <*> f t2
  Filter t -> Filter <$> f t
  At t -> At <$> f t
  SetAt t -> SetAt <$> f t
  Elem t -> Elem <$> f t
  Sum -> return Sum
  Product -> return Product
  ModSum -> return ModSum
  ModProduct -> return ModProduct
  Min1 t -> Min1 <$> f t
  Max1 t -> Max1 <$> f t
  ArgMin t -> ArgMin <$> f t
  ArgMax t -> ArgMax <$> f t
  All -> return All
  Any -> return Any
  Sorted t -> Sorted <$> f t
  Reversed t -> Reversed <$> f t
  Range1 -> return Range1
  Range2 -> return Range2
  Range3 -> return Range3
  -- tuple functions
  Tuple ts -> Tuple <$> mapM f ts
  Proj ts n -> Proj <$> mapM f ts <*> pure n
  -- comparison
  LessThan t -> LessThan <$> f t
  LessEqual t -> LessEqual <$> f t
  GreaterThan t -> GreaterThan <$> f t
  GreaterEqual t -> GreaterEqual <$> f t
  Equal t -> Equal <$> f t
  NotEqual t -> NotEqual <$> f t
  -- combinational functions
  Fact -> return Fact
  Choose -> return Choose
  Permute -> return Permute
  MultiChoose -> return MultiChoose
  -- data structures
  ConvexHullTrickInit -> return ConvexHullTrickInit
  ConvexHullTrickInsert -> return ConvexHullTrickInsert
  ConvexHullTrickGetMin -> return ConvexHullTrickGetMin
  SegmentTreeInitList semigrp -> return $ SegmentTreeInitList semigrp
  SegmentTreeGetRange semigrp -> return $ SegmentTreeGetRange semigrp
  SegmentTreeSetPoint semigrp -> return $ SegmentTreeSetPoint semigrp

mapTypeLiteralM :: Monad m => (Type -> m Type) -> Literal -> m Literal
mapTypeLiteralM f = \case
  LitBuiltin builtin -> LitBuiltin <$> mapTypeBuiltinM f builtin
  LitInt n -> return $ LitInt n
  LitBool p -> return $ LitBool p
  LitNil t -> LitNil <$> f t
  LitBottom t err -> LitBottom <$> f t <*> pure err

mapTypeExprM :: Monad m => (Type -> m Type) -> Expr -> m Expr
mapTypeExprM f = go
  where
    go = \case
      Var x -> return $ Var x
      Lit lit -> Lit <$> mapTypeLiteralM f lit
      App f e -> App <$> go f <*> go e
      Lam x t body -> Lam x <$> f t <*> go body
      Let x t e1 e2 -> Let x <$> f t <*> go e1 <*> go e2

mapTypeExpr :: (Type -> Type) -> Expr -> Expr
mapTypeExpr f e = runIdentity (mapTypeExprM (return . f) e)

mapTypeToplevelExprM :: Monad m => (Type -> m Type) -> ToplevelExpr -> m ToplevelExpr
mapTypeToplevelExprM f = \case
  ResultExpr e -> ResultExpr <$> mapTypeExprM f e
  ToplevelLet x t e cont -> ToplevelLet x <$> f t <*> mapTypeExprM f e <*> mapTypeToplevelExprM f cont
  ToplevelLetRec g args ret body cont -> ToplevelLetRec g <$> mapM (\(x, t) -> (x,) <$> f t) args <*> f ret <*> mapTypeExprM f body <*> mapTypeToplevelExprM f cont

mapTypeProgramM :: Monad m => (Type -> m Type) -> Program -> m Program
mapTypeProgramM = mapTypeToplevelExprM

mapTypeProgram :: (Type -> Type) -> Program -> Program
mapTypeProgram f prog = runIdentity (mapTypeProgramM (return . f) prog)

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

-- | `replaceLenF` replaces @len(f)@ in an expr with @i + k@.
-- * This assumes that there are no name conflicts.
replaceLenF :: MonadError Error m => VarName -> VarName -> Integer -> Expr -> m Expr
replaceLenF f i k = go
  where
    go = \case
      Len' _ (Var f') | f' == f -> return $ Plus' (Var i) (LitInt' k)
      Var y -> return $ Var y
      Lit lit -> return $ Lit lit
      App g e -> App <$> go g <*> go e
      Lam x _ _ | x == i -> throwInternalError "Jikka.Core.Language.Util.replaceLenF: name conflict"
      Lam x t body -> Lam x t <$> (if x == f then return body else go body)
      Let y _ _ _ | y == i -> throwInternalError "Jikka.Core.Language.Util.replaceLenF: name conflict"
      Let y t e1 e2 -> Let y t <$> go e1 <*> (if y == f then return e2 else go e2)

-- | `getRecurrenceFormulaBase` makes a pair @((a_0, ..., a_{k - 1}), a)@ from @setat (... (setat a 0 a_0) ...) (k - 1) a_{k - 1})@.
getRecurrenceFormulaBase :: Expr -> ([Expr], Expr)
getRecurrenceFormulaBase = go (V.replicate recurrenceLimit Nothing)
  where
    recurrenceLimit :: Num a => a
    recurrenceLimit = 20
    go :: V.Vector (Maybe (Expr, Type)) -> Expr -> ([Expr], Expr)
    go base = \case
      SetAt' t e (LitInt' i) e'
        | 0 <= i && i < recurrenceLimit -> go (base V.// [(fromInteger i, Just (e', t))]) e
        | otherwise -> second (\e -> SetAt' t e (LitInt' i) e') $ go base e
      e ->
        let (base', base'') = span isJust (V.toList base)
            base''' = map (fst . fromJust) base'
            e'' = foldr (\(i, e') e -> maybe id (\(e', t) e -> SetAt' t e (LitInt' i) e') e' e) e (zip [toInteger (length base') ..] base'')
         in (base''', e'')

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
