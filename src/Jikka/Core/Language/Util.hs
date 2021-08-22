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

mapTypeLiteralM :: Monad m => (Type -> m Type) -> Literal -> m Literal
mapTypeLiteralM f = \case
  LitBuiltin builtin ts -> LitBuiltin builtin <$> mapM f ts
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
      Assert e1 e2 -> Assert <$> go e1 <*> go e2

mapTypeExpr :: (Type -> Type) -> Expr -> Expr
mapTypeExpr f e = runIdentity (mapTypeExprM (return . f) e)

mapTypeToplevelExprM :: Monad m => (Type -> m Type) -> ToplevelExpr -> m ToplevelExpr
mapTypeToplevelExprM f = \case
  ResultExpr e -> ResultExpr <$> mapTypeExprM f e
  ToplevelLet x t e cont -> ToplevelLet x <$> f t <*> mapTypeExprM f e <*> mapTypeToplevelExprM f cont
  ToplevelLetRec g args ret body cont -> ToplevelLetRec g <$> mapM (\(x, t) -> (x,) <$> f t) args <*> f ret <*> mapTypeExprM f body <*> mapTypeToplevelExprM f cont
  ToplevelAssert e cont -> ToplevelAssert <$> mapTypeExprM f e <*> mapTypeToplevelExprM f cont

mapTypeProgramM :: Monad m => (Type -> m Type) -> Program -> m Program
mapTypeProgramM = mapTypeToplevelExprM

mapTypeProgram :: (Type -> Type) -> Program -> Program
mapTypeProgram f prog = runIdentity (mapTypeProgramM (return . f) prog)

-- | `mapSubExprM'` substitutes exprs using given two functions, which are called in pre-order and post-order.
mapSubExprM' :: Monad m => ([(VarName, Type)] -> Expr -> m Expr) -> ([(VarName, Type)] -> Expr -> m Expr) -> [(VarName, Type)] -> Expr -> m Expr
mapSubExprM' pre post env e = do
  e <- pre env e
  let go = mapSubExprM' pre post
  e <- case e of
    Var y -> return $ Var y
    Lit lit -> return $ Lit lit
    App g e -> App <$> go env g <*> go env e
    Lam x t body -> Lam x t <$> go ((x, t) : env) body
    Let y t e1 e2 -> Let y t <$> go env e1 <*> go ((y, t) : env) e2
    Assert e1 e2 -> Assert <$> go env e1 <*> go env e2
  post env e

mapToplevelExprM' :: Monad m => ([(VarName, Type)] -> ToplevelExpr -> m ToplevelExpr) -> ([(VarName, Type)] -> ToplevelExpr -> m ToplevelExpr) -> [(VarName, Type)] -> ToplevelExpr -> m ToplevelExpr
mapToplevelExprM' pre post env e = do
  e <- pre env e
  e <- case e of
    ResultExpr e -> return $ ResultExpr e
    ToplevelLet y t e cont ->
      ToplevelLet y t e <$> mapToplevelExprM' pre post ((y, t) : env) cont
    ToplevelLetRec g args ret body cont ->
      let env' = (g, foldr (FunTy . snd) ret args) : env
       in ToplevelLetRec g args ret body <$> mapToplevelExprM' pre post env' cont
    ToplevelAssert e cont ->
      ToplevelAssert e <$> mapToplevelExprM' pre post env cont
  post env e

mapExprToplevelExprM :: Monad m => ([(VarName, Type)] -> Expr -> m Expr) -> [(VarName, Type)] -> ToplevelExpr -> m ToplevelExpr
mapExprToplevelExprM f env = mapToplevelExprM' pre' (\_ e -> return e) env
  where
    pre' env = \case
      ResultExpr e -> ResultExpr <$> f env e
      ToplevelLet y t e cont -> ToplevelLet y t <$> f env e <*> pure cont
      ToplevelLetRec g args ret body cont ->
        let env' = (g, foldr (FunTy . snd) ret args) : env
         in ToplevelLetRec g args ret <$> f (reverse args ++ env') body <*> pure cont
      ToplevelAssert e cont -> ToplevelAssert <$> f env e <*> pure cont

mapExprProgramM :: Monad m => ([(VarName, Type)] -> Expr -> m Expr) -> Program -> m Program
mapExprProgramM f = mapExprToplevelExprM f []

-- | `mapSubExprM` is a wrapper of `mapSubExprM'`. This function works in post-order.
mapSubExprM :: Monad m => ([(VarName, Type)] -> Expr -> m Expr) -> [(VarName, Type)] -> Expr -> m Expr
mapSubExprM f = mapSubExprM' (\_ e -> return e) f

mapSubExpr :: ([(VarName, Type)] -> Expr -> Expr) -> [(VarName, Type)] -> Expr -> Expr
mapSubExpr f env e = runIdentity $ mapSubExprM (\env e -> return $ f env e) env e

mapExprToplevelExpr :: ([(VarName, Type)] -> Expr -> Expr) -> [(VarName, Type)] -> ToplevelExpr -> ToplevelExpr
mapExprToplevelExpr f env e = runIdentity $ mapExprToplevelExprM (\env e -> return $ f env e) env e

-- | @mapExprProgram f prog@ applies @f@ to each root exprs in @prog@.
-- This doesn't run into sub-exprs. For example, @toplevel-let x = (e1 + e2) in ...@ becomes @toplevel-let x = (f (e1 + e2)) in ...@, instead of @toplevel-let x = (f (f e1 + f e2)) in ...@
mapExprProgram :: ([(VarName, Type)] -> Expr -> Expr) -> Program -> Program
mapExprProgram f prog = runIdentity $ mapExprProgramM (\env e -> return $ f env e) prog

-- | `mapToplevelExprM` is a wrapper of `mapToplevelExprM'`. This function works in post-order.
mapToplevelExprM :: Monad m => ([(VarName, Type)] -> ToplevelExpr -> m ToplevelExpr) -> [(VarName, Type)] -> ToplevelExpr -> m ToplevelExpr
mapToplevelExprM f env e = mapToplevelExprM' (\_ e -> return e) f env e

mapToplevelExprProgramM :: Monad m => ([(VarName, Type)] -> Program -> m Program) -> Program -> m Program
mapToplevelExprProgramM f prog = mapToplevelExprM f [] prog

mapToplevelExprProgram :: ([(VarName, Type)] -> Program -> Program) -> Program -> Program
mapToplevelExprProgram f prog = runIdentity $ mapToplevelExprProgramM (\env e -> return $ f env e) prog

listSubExprs :: Expr -> [Expr]
listSubExprs e = getDual . execWriter $ mapSubExprM go [] e
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
  JustDiv -> True
  Pow -> True
  -- advanced arithmetical functions
  Abs -> True
  Gcd -> True
  Lcm -> True
  Min2 -> True
  Max2 -> True
  Iterate -> False
  -- logical functions
  Not -> True
  And -> True
  Or -> True
  Implies -> True
  If -> True
  -- bitwise functions
  BitNot -> True
  BitAnd -> True
  BitOr -> True
  BitXor -> True
  BitLeftShift -> True
  BitRightShift -> True
  -- matrix functions
  MatAp _ _ -> True
  MatZero _ _ -> True
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
  Cons -> False
  Snoc -> False
  Foldl -> False
  Scanl -> False
  Build -> False
  Len -> True
  Map -> False
  Filter -> False
  At -> True
  SetAt -> False
  Elem -> False
  Sum -> False
  Product -> False
  ModSum -> False
  ModProduct -> False
  Min1 -> False
  Max1 -> False
  ArgMin -> False
  ArgMax -> False
  Gcd1 -> False
  Lcm1 -> False
  All -> False
  Any -> False
  Sorted -> False
  Reversed -> False
  Range1 -> False
  Range2 -> False
  Range3 -> False
  -- tuple functions
  Tuple -> True
  Proj _ -> True
  -- comparison
  LessThan -> True
  LessEqual -> True
  GreaterThan -> True
  GreaterEqual -> True
  Equal -> True
  NotEqual -> True
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
    (Lit (LitBuiltin f _), args) -> isConstantTimeBuiltin f && all isConstantTimeExpr args
    _ -> False
  Lam _ _ _ -> True
  Let _ _ e1 e2 -> isConstantTimeExpr e1 && isConstantTimeExpr e2
  Assert e1 e2 -> isConstantTimeExpr e1 && isConstantTimeExpr e2

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
      Assert e1 e2 -> Assert <$> go e1 <*> go e2

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
