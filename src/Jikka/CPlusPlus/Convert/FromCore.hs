{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Jikka.CPlusPlus.Convert.FromCore
-- Description : converts core programs to C++ programs. / core 言語のプログラムを C++ のプログラムに変換します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.CPlusPlus.FromCore` converts exprs of our core language to exprs of C++.
module Jikka.CPlusPlus.Convert.FromCore
  ( run,
  )
where

import Control.Monad.Writer.Strict
import qualified Jikka.CPlusPlus.Language.Expr as Y
import qualified Jikka.CPlusPlus.Language.Util as Y
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Format as X (formatBuiltinIsolated, formatType)
import qualified Jikka.Core.Language.Expr as X
import qualified Jikka.Core.Language.LambdaPatterns as X
import qualified Jikka.Core.Language.TypeCheck as X
import qualified Jikka.Core.Language.Util as X

--------------------------------------------------------------------------------
-- monad

renameVarName' :: MonadAlpha m => Y.NameKind -> X.VarName -> m Y.VarName
renameVarName' kind x = Y.renameVarName kind (X.unVarName x)

type Env = [(X.VarName, X.Type, Y.VarName)]

typecheckExpr :: MonadError Error m => Env -> X.Expr -> m X.Type
typecheckExpr env = X.typecheckExpr (map (\(x, t, _) -> (x, t)) env)

lookupVarName :: MonadError Error m => Env -> X.VarName -> m Y.VarName
lookupVarName env x = case lookup x (map (\(x, _, y) -> (x, y)) env) of
  Just y -> return y
  Nothing -> throwInternalError $ "undefined variable: " ++ X.unVarName x

class Monad m => MonadStatements m where
  useStatement :: Y.Statement -> m ()

instance Monad m => MonadStatements (WriterT (Dual [Y.Statement]) m) where
  useStatement stmt = tell $ Dual [stmt]

useStatements :: MonadStatements m => [Y.Statement] -> m ()
useStatements = mapM_ useStatement

runStatementsT :: Monad m => WriterT (Dual [Y.Statement]) m a -> m ([Y.Statement], a)
runStatementsT f = do
  (a, stmts) <- runWriterT f
  return (reverse (getDual stmts), a)

--------------------------------------------------------------------------------
-- run

runType :: MonadError Error m => X.Type -> m Y.Type
runType = \case
  t@X.VarTy {} -> throwInternalError $ "cannot convert type variable: " ++ X.formatType t
  X.IntTy -> return Y.TyInt64
  X.BoolTy -> return Y.TyBool
  X.ListTy t -> Y.TyVector <$> runType t
  X.TupleTy ts -> do
    ts <- mapM runType ts
    return $
      if Y.shouldBeArray ts
        then Y.TyArray (head ts) (fromIntegral (length ts))
        else Y.TyTuple ts
  X.FunTy t ret -> Y.TyFunction <$> runType ret <*> mapM runType [t]
  X.DataStructureTy ds -> case ds of
    X.ConvexHullTrick -> return Y.TyConvexHullTrick
    X.SegmentTree semigrp -> return $ Y.TySegmentTree (runSemigroup semigrp)

runSemigroup :: X.Semigroup' -> Y.Monoid'
runSemigroup = \case
  X.SemigroupIntPlus -> Y.MonoidIntPlus
  X.SemigroupIntMin -> Y.MonoidIntMin
  X.SemigroupIntMax -> Y.MonoidIntMax
  X.SemigroupIntGcd -> Y.MonoidIntGcd
  X.SemigroupIntLcm -> Y.MonoidIntLcm

runLiteral :: (MonadAlpha m, MonadError Error m) => Env -> X.Literal -> m Y.Expr
runLiteral env = \case
  X.LitBuiltin builtin ts -> do
    (stmts, e) <- runStatementsT $ runAppBuiltin env builtin ts []
    case stmts of
      [] -> return e
      _ -> throwInternalError "now builtin values don't use statements"
  X.LitInt n
    | - (2 ^ 63) <= n && n < 2 ^ 63 -> return $ Y.Lit (Y.LitInt64 n)
    | otherwise -> throwInternalError $ "integer value is too large for int64_t: " ++ show n
  X.LitBool p -> return $ Y.Lit (Y.LitBool p)
  X.LitNil t -> do
    t <- runType t
    return $ Y.vecCtor t []
  X.LitBottom t err -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::error" [t]) [Y.Lit (Y.LitString err)]

arityOfBuiltin :: MonadError Error m => X.Builtin -> [X.Type] -> m Int
arityOfBuiltin builtin ts = case builtin of
  X.Min2 -> return 2
  X.Max2 -> return 2
  X.Foldl -> return 3
  X.Iterate -> return 3
  X.At -> return 2
  X.Min1 -> return 1
  X.Max1 -> return 1
  X.Proj _ -> return 1
  builtin -> length . fst . X.uncurryFunTy <$> X.builtinToType builtin ts

runIterate :: (MonadStatements m, MonadAlpha m, MonadError Error m) => Env -> X.Type -> X.Expr -> X.Expr -> X.Expr -> m Y.Expr
runIterate env t n f x = do
  t <- runType t
  n <- runExpr env n
  x <- runExpr env x
  y <- Y.newFreshName Y.LocalNameKind
  i <- Y.newFreshName Y.LoopCounterNameKind
  (stmtsF, body, f) <- runExprFunction env f (Y.Var y)
  useStatement $ Y.Declare t y (Y.DeclareCopy x)
  useStatements stmtsF
  useStatement $ Y.repStatement i (Y.cast Y.TyInt32 n) (body ++ [Y.assignSimple y f])
  return $ Y.Var y

runIf :: (MonadStatements m, MonadAlpha m, MonadError Error m) => Env -> X.Type -> X.Expr -> X.Expr -> X.Expr -> m Y.Expr
runIf env t e1 e2 e3 = do
  e1' <- runExpr env e1
  (stmts2, e2') <- runStatementsT $ runExpr env e2
  (stmts3, e3') <- runStatementsT $ runExpr env e3
  case (stmts2, stmts3) of
    ([], [])
      | X.isConstantTimeExpr e2 && X.isConstantTimeExpr e3 ->
        return $ Y.Cond e1' e2' e3'
    _ -> do
      t <- runType t
      phi <- Y.newFreshName Y.LocalNameKind
      let assign = Y.Assign . Y.AssignExpr Y.SimpleAssign (Y.LeftVar phi)
      useStatement $ Y.Declare t phi Y.DeclareDefault
      useStatement $ Y.If e1' (stmts2 ++ [assign e2']) (Just (stmts3 ++ [assign e3']))
      return $ Y.Var phi

runFoldl :: (MonadStatements m, MonadAlpha m, MonadError Error m) => Env -> X.Type -> X.Type -> X.Expr -> X.Expr -> X.Expr -> m Y.Expr
runFoldl env t1 t2 f init xs = do
  init <- runExpr env init
  xs <- runExpr env xs
  t1 <- runType t1
  t2 <- runType t2
  y <- Y.newFreshName Y.LocalNameKind
  x <- Y.newFreshName Y.LocalNameKind
  (stmtsF, body, f) <- runExprFunction2 env f (Y.Var y) (Y.Var x)
  useStatement $ Y.Declare t2 y (Y.DeclareCopy init)
  useStatements stmtsF
  useStatement $ Y.ForEach t1 x xs (body ++ [Y.assignSimple y f])
  return $ Y.Var y

runMap :: (MonadStatements m, MonadAlpha m, MonadError Error m) => Env -> X.Type -> X.Type -> X.Expr -> X.Expr -> m Y.Expr
runMap env _ t2 f xs = do
  ys <- Y.newFreshName Y.LocalNameKind
  t2 <- runType t2
  case (f, xs) of
    -- optimize @map (const e) xs@
    (X.LamConst _ e, xs) -> do
      xs <- runExpr env xs
      e <- runExpr env e
      useStatement $ Y.Declare (Y.TyVector t2) ys (Y.DeclareCopy (Y.vecCtor t2 [Y.size xs, e]))
      return $ Y.Var ys
    -- other cases
    _ -> do
      xs <- runExpr env xs
      i <- Y.newFreshName Y.LoopCounterNameKind
      (stmtsF, body, f) <- runExprFunction env f (Y.at xs (Y.Var i))
      useStatement $ Y.Declare (Y.TyVector t2) ys (Y.DeclareCopy (Y.vecCtor t2 [Y.size xs]))
      useStatements stmtsF
      useStatement $ Y.repStatement i (Y.cast Y.TyInt32 (Y.size xs)) (body ++ [Y.assignAt ys (Y.Var i) f])
      return $ Y.Var ys

runAppBuiltin :: (MonadStatements m, MonadAlpha m, MonadError Error m) => Env -> X.Builtin -> [X.Type] -> [X.Expr] -> m Y.Expr
runAppBuiltin env f ts args = wrapError' ("converting builtin " ++ X.formatBuiltinIsolated f ts) $ do
  let go0T :: (MonadAlpha m, MonadError Error m, MonadStatements m) => m Y.Expr -> m Y.Expr
      go0T f = case ts of
        [] -> f
        _ -> throwInternalError $ "expected 0 type arguments, got " ++ show (length ts)
  let go1T' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (X.Type -> m Y.Expr) -> m Y.Expr
      go1T' f = case ts of
        [t1] -> f t1
        _ -> throwInternalError $ "expected 1 type argument, got " ++ show (length ts)
  let go1T :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Type -> m Y.Expr) -> m Y.Expr
      go1T f = go1T' $ f <=< runType
  let go2T' f = case ts of
        [t1, t2] -> f t1 t2
        _ -> throwInternalError $ "expected 2 type arguments, got " ++ show (length ts)
  let go0E :: (MonadAlpha m, MonadError Error m, MonadStatements m) => m Y.Expr -> m Y.Expr
      go0E f = case args of
        [] -> f
        _ -> throwInternalError $ "expected 0 type arguments, got " ++ show (length args)
  let go1E' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (X.Expr -> m Y.Expr) -> m Y.Expr
      go1E' f = case args of
        [e1] -> f e1
        _ -> throwInternalError $ "expected 1 type argument, got " ++ show (length args)
  let go1E :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Expr -> m Y.Expr) -> m Y.Expr
      go1E f = go1E' $ f <=< runExpr env
  let go2E' f = case args of
        [e1, e2] -> f e1 e2
        _ -> throwInternalError $ "expected 2 type arguments, got " ++ show (length args)
  let go2E f = go2E' $ \e1 e2 -> join $ f <$> runExpr env e1 <*> runExpr env e2
  let go3E' f = case args of
        [e1, e2, e3] -> f e1 e2 e3
        _ -> throwInternalError $ "expected 2 type arguments, got " ++ show (length args)
  let go3E f = go3E' $ \e1 e2 e3 -> join $ f <$> runExpr env e1 <*> runExpr env e2 <*> runExpr env e3
  let go00 f = go0T $ go0E $ return f
  let go01' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Expr -> m Y.Expr) -> m Y.Expr
      go01' f = go0T $ go1E f
  let go01 :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Expr -> Y.Expr) -> m Y.Expr
      go01 f = go0T $ go1E $ \e1 -> return $ f e1
  let go11' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Type -> Y.Expr -> m Y.Expr) -> m Y.Expr
      go11' f = go1T $ \t1 -> go1E $ \e1 -> f t1 e1
  let go11 :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Type -> Y.Expr -> Y.Expr) -> m Y.Expr
      go11 f = go1T $ \t1 -> go1E $ \e1 -> return $ f t1 e1
  let go02' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Expr -> Y.Expr -> m Y.Expr) -> m Y.Expr
      go02' f = go0T $ go2E f
  let go02 :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Expr -> Y.Expr -> Y.Expr) -> m Y.Expr
      go02 f = go0T $ go2E $ \e1 e2 -> return $ f e1 e2
  let go12'' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (X.Type -> X.Expr -> X.Expr -> m Y.Expr) -> m Y.Expr
      go12'' f = go1T' $ \t1 -> go2E' $ \e1 e2 -> f t1 e1 e2
  let go12' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Type -> Y.Expr -> Y.Expr -> m Y.Expr) -> m Y.Expr
      go12' f = go1T $ \t1 -> go2E $ \e1 e2 -> f t1 e1 e2
  let go12 :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Type -> Y.Expr -> Y.Expr -> Y.Expr) -> m Y.Expr
      go12 f = go1T $ \t1 -> go2E $ \e1 e2 -> return $ f t1 e1 e2
  let go22'' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (X.Type -> X.Type -> X.Expr -> X.Expr -> m Y.Expr) -> m Y.Expr
      go22'' f = go2T' $ \t1 t2 -> go2E' $ \e1 e2 -> f t1 t2 e1 e2
  let go03' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Expr -> Y.Expr -> Y.Expr -> m Y.Expr) -> m Y.Expr
      go03' f = go0T $ go3E f
  let go03 f = go0T $ go3E $ \e1 e2 e3 -> return $ f e1 e2 e3
  let go13'' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (X.Type -> X.Expr -> X.Expr -> X.Expr -> m Y.Expr) -> m Y.Expr
      go13'' f = go1T' $ \t1 -> go3E' $ \e1 e2 e3 -> f t1 e1 e2 e3
  let go13' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (Y.Type -> Y.Expr -> Y.Expr -> Y.Expr -> m Y.Expr) -> m Y.Expr
      go13' f = go1T $ \t1 -> go3E $ \e1 e2 e3 -> f t1 e1 e2 e3
  let go23'' :: (MonadAlpha m, MonadError Error m, MonadStatements m) => (X.Type -> X.Type -> X.Expr -> X.Expr -> X.Expr -> m Y.Expr) -> m Y.Expr
      go23'' f = go2T' $ \t1 t2 -> go3E' $ \e1 e2 e3 -> f t1 t2 e1 e2 e3
  let goN1 :: (MonadAlpha m, MonadError Error m, MonadStatements m) => ([Y.Type] -> Y.Expr -> Y.Expr) -> m Y.Expr
      goN1 f = case args of
        [e1] -> do
          ts <- mapM runType ts
          e1 <- runExpr env e1
          return $ f ts e1
        _ -> throwInternalError $ "expected 1 argument, got " ++ show (length args)
  let goNN :: (MonadAlpha m, MonadError Error m, MonadStatements m) => ([Y.Type] -> [Y.Expr] -> Y.Expr) -> m Y.Expr
      goNN f = do
        ts <- mapM runType ts
        args <- mapM (runExpr env) args
        return $ f ts args
  case f of
    -- arithmetical functions
    X.Negate -> go01 $ \e -> Y.UnOp Y.Negate e
    X.Plus -> go02 $ \e1 e2 -> Y.BinOp Y.Add e1 e2
    X.Minus -> go02 $ \e1 e2 -> Y.BinOp Y.Sub e1 e2
    X.Mult -> go02 $ \e1 e2 -> Y.BinOp Y.Mul e1 e2
    X.FloorDiv -> go02 $ \e1 e2 -> Y.Call (Y.Function "jikka::floordiv" []) [e1, e2]
    X.FloorMod -> go02 $ \e1 e2 -> Y.Call (Y.Function "jikka::floormod" []) [e1, e2]
    X.CeilDiv -> go02 $ \e1 e2 -> Y.Call (Y.Function "jikka::ceildiv" []) [e1, e2]
    X.CeilMod -> go02 $ \e1 e2 -> Y.Call (Y.Function "jikka::ceilmod" []) [e1, e2]
    X.Pow -> go02 $ \e1 e2 -> Y.Call (Y.Function "jikka::notmod::pow" []) [e1, e2]
    -- advanced arithmetical functions
    X.Abs -> go01 $ \e -> Y.Call (Y.Function "std::abs" []) [e]
    X.Gcd -> go02 $ \e1 e2 -> Y.Call (Y.Function "std::gcd" []) [e1, e2]
    X.Lcm -> go02 $ \e1 e2 -> Y.Call (Y.Function "std::lcm" []) [e1, e2]
    X.Min2 -> go12 $ \t e1 e2 -> Y.Call (Y.Function "std::min" [t]) [e1, e2]
    X.Max2 -> go12 $ \t e1 e2 -> Y.Call (Y.Function "std::max" [t]) [e1, e2]
    X.Iterate -> go13'' $ runIterate env
    -- logical functions
    X.Not -> go01 $ \e -> Y.UnOp Y.Not e
    X.And -> go02 $ \e1 e2 -> Y.BinOp Y.And e1 e2
    X.Or -> go02 $ \e1 e2 -> Y.BinOp Y.Or e1 e2
    X.Implies -> go02 $ \e1 e2 -> Y.BinOp Y.Or (Y.UnOp Y.Not e1) e2
    X.If -> go13'' $ runIf env
    -- bitwise functions
    X.BitNot -> go01 $ \e -> Y.UnOp Y.BitNot e
    X.BitAnd -> go02 $ \e1 e2 -> Y.BinOp Y.BitAnd e1 e2
    X.BitOr -> go02 $ \e1 e2 -> Y.BinOp Y.BitOr e1 e2
    X.BitXor -> go02 $ \e1 e2 -> Y.BinOp Y.BitXor e1 e2
    X.BitLeftShift -> go02 $ \e1 e2 -> Y.BinOp Y.BitLeftShift e1 e2
    X.BitRightShift -> go02 $ \e1 e2 -> Y.BinOp Y.BitRightShift e1 e2
    -- matrix functions
    X.MatAp h w -> go02 $ \f x -> Y.Call (Y.Function "jikka::mat::ap" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral w)]) [f, x]
    X.MatZero n -> go00 $ Y.Call (Y.Function "jikka::mat::zero" [Y.TyIntValue (fromIntegral n)]) []
    X.MatOne n -> go00 $ Y.Call (Y.Function "jikka::mat::one" [Y.TyIntValue (fromIntegral n)]) []
    X.MatAdd h w -> go02 $ \f g -> Y.Call (Y.Function "jikka::mat::add" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral w)]) [f, g]
    X.MatMul h n w -> go02 $ \f g -> Y.Call (Y.Function "jikka::mat::mul" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral n), Y.TyIntValue (fromIntegral w)]) [f, g]
    X.MatPow n -> go02 $ \f k -> Y.Call (Y.Function "jikka::mat::pow" [Y.TyIntValue (fromIntegral n)]) [f, k]
    X.VecFloorMod n -> go02 $ \x m -> Y.Call (Y.Function "jikka::modmat::floormod" [Y.TyIntValue (fromIntegral n)]) [x, m]
    X.MatFloorMod h w -> go02 $ \f m -> Y.Call (Y.Function "jikka::modmat::floormod" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral w)]) [f, m]
    -- modular functions
    X.ModNegate -> go02 $ \e1 e2 -> Y.Call (Y.Function "jikka::mod::negate" []) [e1, e2]
    X.ModPlus -> go03 $ \e1 e2 e3 -> Y.Call (Y.Function "jikka::mod::plus" []) [e1, e2, e3]
    X.ModMinus -> go03 $ \e1 e2 e3 -> Y.Call (Y.Function "jikka::mod::minus" []) [e1, e2, e3]
    X.ModMult -> go03 $ \e1 e2 e3 -> Y.Call (Y.Function "jikka::mod::mult" []) [e1, e2, e3]
    X.ModInv -> go02 $ \e1 e2 -> Y.Call (Y.Function "jikka::mod::inv" []) [e1, e2]
    X.ModPow -> go03 $ \e1 e2 e3 -> Y.Call (Y.Function "jikka::mod::pow" []) [e1, e2, e3]
    X.ModMatAp h w -> go03 $ \f x m -> Y.Call (Y.Function "jikka::modmat::ap" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral w)]) [f, x, m]
    X.ModMatAdd h w -> go03 $ \f g m -> Y.Call (Y.Function "jikka::modmat::add" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral w)]) [f, g, m]
    X.ModMatMul h n w -> go03 $ \f g m -> Y.Call (Y.Function "jikka::modmat::mul" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral n), Y.TyIntValue (fromIntegral w)]) [f, g, m]
    X.ModMatPow n -> go03 $ \f k m -> Y.Call (Y.Function "jikka::modmat::pow" [Y.TyIntValue (fromIntegral n)]) [f, k, m]
    -- list functions
    X.Cons -> go12' $ \t x xs -> do
      ys <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare (Y.TyVector t) ys Y.DeclareDefault
      useStatement $ Y.callMethod' (Y.Var ys) "push_back" [x]
      useStatement $ Y.callMethod' (Y.Var ys) "insert" [Y.end (Y.Var ys), Y.begin xs, Y.end xs]
      return $ Y.Var ys
    X.Snoc -> go12' $ \t xs x -> do
      ys <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare (Y.TyVector t) ys (Y.DeclareCopy xs)
      useStatement $ Y.callMethod' (Y.Var ys) "push_back" [x]
      return $ Y.Var ys
    X.Foldl -> go23'' $ runFoldl env
    X.Scanl -> go23'' $ \_ t2 f init xs -> do
      init <- runExpr env init
      xs <- runExpr env xs
      t2 <- runType t2
      ys <- Y.newFreshName Y.LocalNameKind
      i <- Y.newFreshName Y.LoopCounterNameKind
      (stmtsF, body, f) <- runExprFunction2 env f (Y.at (Y.Var ys) (Y.Var i)) (Y.at xs (Y.Var i))
      useStatement $ Y.Declare (Y.TyVector t2) ys (Y.DeclareCopy (Y.vecCtor t2 [Y.incrExpr (Y.size xs)]))
      useStatement $ Y.assignAt ys (Y.litInt32 0) init
      useStatements stmtsF
      useStatement $ Y.repStatement i (Y.cast Y.TyInt32 (Y.size xs)) (body ++ [Y.assignAt ys (Y.incrExpr (Y.Var i)) f])
      return $ Y.Var ys
    X.Build -> go13'' $ \t f xs n -> do
      xs <- runExpr env xs
      n <- runExpr env n
      t <- runType t
      ys <- Y.newFreshName Y.LocalNameKind
      i <- Y.newFreshName Y.LoopCounterNameKind
      (stmtsF, body, f) <- runExprFunction env f (Y.Var ys)
      useStatement $ Y.Declare (Y.TyVector t) ys (Y.DeclareCopy xs)
      useStatements stmtsF
      useStatement $ Y.repStatement i (Y.cast Y.TyInt32 n) (body ++ [Y.callMethod' (Y.Var ys) "push_back" [f]])
      return $ Y.Var ys
    X.Len -> go11 $ \_ e -> Y.cast Y.TyInt64 (Y.size e)
    X.Map -> go22'' $ runMap env
    X.Filter -> go12'' $ \t f xs -> do
      xs <- runExpr env xs
      t <- runType t
      ys <- Y.newFreshName Y.LocalNameKind
      x <- Y.newFreshName Y.LocalNameKind
      (stmtsF, body, f) <- runExprFunction env f (Y.Var x)
      useStatement $ Y.Declare (Y.TyVector t) ys Y.DeclareDefault
      useStatements stmtsF
      useStatement $ Y.ForEach t x xs (body ++ [Y.If f [Y.callMethod' (Y.Var ys) "push_back" [Y.Var x]] Nothing])
      return $ Y.Var ys
    X.At -> go12 $ \_ e1 e2 -> Y.at e1 e2
    X.SetAt -> go13' $ \t xs i x -> do
      ys <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare (Y.TyVector t) ys (Y.DeclareCopy xs)
      useStatement $ Y.assignAt ys i x
      return $ Y.Var ys
    X.Elem -> go12' $ \_ xs x -> do
      y <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare Y.TyBool y (Y.DeclareCopy (Y.BinOp Y.NotEqual (Y.callFunction "std::find" [] [Y.begin xs, Y.end xs, x]) (Y.end xs)))
      return $ Y.Var y
    X.Sum -> go01' $ \xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare Y.TyInt64 y (Y.DeclareCopy (Y.callFunction "std::accumulate" [] [Y.begin xs, Y.end xs, Y.litInt64 0]))
      return $ Y.Var y
    X.ModSum -> go02' $ \xs m -> do
      y <- Y.newFreshName Y.LocalNameKind
      x <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare Y.TyInt64 y (Y.DeclareCopy (Y.litInt64 0))
      useStatement $ Y.ForEach Y.TyInt64 x xs [Y.Assign (Y.AssignExpr Y.AddAssign (Y.LeftVar y) (Y.callFunction "jikka::floormod" [] [Y.Var x, m]))]
      return $ Y.callFunction "jikka::floormod" [] [Y.Var y, m]
    X.Product -> go01' $ \xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      x <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare Y.TyInt64 y (Y.DeclareCopy (Y.litInt64 1))
      useStatement $ Y.ForEach Y.TyInt64 x xs [Y.Assign (Y.AssignExpr Y.MulAssign (Y.LeftVar y) (Y.Var x))]
      return $ Y.Var y
    X.ModProduct -> go02' $ \xs m -> do
      y <- Y.newFreshName Y.LocalNameKind
      x <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare Y.TyInt64 y (Y.DeclareCopy (Y.litInt64 1))
      useStatement $ Y.ForEach Y.TyInt64 x xs [Y.Assign (Y.AssignExpr Y.SimpleAssign (Y.LeftVar y) (Y.callFunction "jikka::mod::mult" [] [Y.Var y, Y.Var x, m]))]
      return $ Y.Var y
    X.Min1 -> go11' $ \t xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare t y (Y.DeclareCopy (Y.UnOp Y.Deref (Y.callFunction "std::min_element" [] [Y.begin xs, Y.end xs])))
      return $ Y.Var y
    X.Max1 -> go11' $ \t xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare t y (Y.DeclareCopy (Y.UnOp Y.Deref (Y.callFunction "std::max_element" [] [Y.begin xs, Y.end xs])))
      return $ Y.Var y
    X.ArgMin -> go11' $ \t xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare t y (Y.DeclareCopy (Y.BinOp Y.Sub (Y.callFunction "std::min_element" [] [Y.begin xs, Y.end xs]) (Y.begin xs)))
      return $ Y.Var y
    X.ArgMax -> go11' $ \t xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare t y (Y.DeclareCopy (Y.BinOp Y.Sub (Y.callFunction "std::max_element" [] [Y.begin xs, Y.end xs]) (Y.begin xs)))
      return $ Y.Var y
    X.Gcd1 -> go11' $ \t xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare t y (Y.DeclareCopy (Y.UnOp Y.Deref (Y.callFunction "std::accumulate" [] [Y.begin xs, Y.end xs, Y.litInt64 0, Y.Lam [(Y.TyAuto, Y.VarName "a"), (Y.TyAuto, Y.VarName "b")] Y.TyAuto [Y.Return $ Y.callFunction "std::gcd" [] [Y.Var $ Y.VarName "a", Y.Var $ Y.VarName "b"]]])))
      return $ Y.Var y
    X.Lcm1 -> go11' $ \t xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare t y (Y.DeclareCopy (Y.UnOp Y.Deref (Y.callFunction "std::accumulate" [] [Y.begin xs, Y.end xs, Y.litInt64 1, Y.Lam [(Y.TyAuto, Y.VarName "a"), (Y.TyAuto, Y.VarName "b")] Y.TyAuto [Y.Return $ Y.callFunction "std::lcm" [] [Y.Var $ Y.VarName "a", Y.Var $ Y.VarName "b"]]])))
      return $ Y.Var y
    X.All -> go01' $ \xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare Y.TyBool y (Y.DeclareCopy (Y.BinOp Y.Equal (Y.callFunction "std::find" [] [Y.begin xs, Y.end xs, Y.Lit (Y.LitBool False)]) (Y.end xs)))
      return $ Y.Var y
    X.Any -> go01' $ \xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare Y.TyBool y (Y.DeclareCopy (Y.BinOp Y.NotEqual (Y.callFunction "std::find" [] [Y.begin xs, Y.end xs, Y.Lit (Y.LitBool True)]) (Y.end xs)))
      return $ Y.Var y
    X.Sorted -> go11' $ \t xs -> do
      ys <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare (Y.TyVector t) ys (Y.DeclareCopy xs)
      useStatement $ Y.callFunction' "std::sort" [] [Y.begin (Y.Var ys), Y.end (Y.Var ys)]
      return $ Y.Var ys
    X.Reversed -> go11' $ \t xs -> do
      ys <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare (Y.TyVector t) ys (Y.DeclareCopy xs)
      useStatement $ Y.callFunction' "std::reverse" [] [Y.begin (Y.Var ys), Y.end (Y.Var ys)]
      return $ Y.Var ys
    X.Range1 -> go01 $ \n -> Y.Call Y.Range [n]
    X.Range2 -> go02' $ \from to -> do
      ys <- Y.newFreshName Y.LocalNameKind
      useStatement $ Y.Declare (Y.TyVector Y.TyInt64) ys (Y.DeclareCopy (Y.vecCtor Y.TyInt64 [Y.BinOp Y.Sub to from]))
      useStatement $ Y.callFunction' "std::iota" [] [Y.begin (Y.Var ys), Y.end (Y.Var ys), from]
      return $ Y.Var ys
    X.Range3 -> go03' $ \from to step -> do
      ys <- Y.newFreshName Y.LocalNameKind
      i <- Y.newFreshName Y.LoopCounterNameKind
      useStatement $ Y.Declare (Y.TyVector Y.TyInt64) ys Y.DeclareDefault
      useStatement $ Y.For Y.TyInt32 i from (Y.BinOp Y.LessThan (Y.Var i) to) (Y.AssignExpr Y.AddAssign (Y.LeftVar i) step) [Y.callMethod' (Y.Var ys) "push_back" [Y.Var i]]
      return $ Y.Var ys
    -- tuple functions
    X.Tuple -> goNN $ \ts es ->
      if Y.shouldBeArray ts
        then Y.Call (Y.ArrayExt (head ts)) es
        else Y.Call (Y.StdTuple ts) es
    X.Proj n -> goN1 $ \ts e ->
      if Y.shouldBeArray ts
        then Y.at e (Y.Lit (Y.LitInt32 (fromIntegral n)))
        else Y.Call (Y.StdGet (toInteger n)) [e]
    -- comparison
    X.LessThan -> go12 $ \_ e1 e2 -> Y.BinOp Y.LessThan e1 e2
    X.LessEqual -> go12 $ \_ e1 e2 -> Y.BinOp Y.LessEqual e1 e2
    X.GreaterThan -> go12 $ \_ e1 e2 -> Y.BinOp Y.GreaterThan e1 e2
    X.GreaterEqual -> go12 $ \_ e1 e2 -> Y.BinOp Y.GreaterEqual e1 e2
    X.Equal -> go12 $ \_ e1 e2 -> Y.BinOp Y.Equal e1 e2
    X.NotEqual -> go12 $ \_ e1 e2 -> Y.BinOp Y.NotEqual e1 e2
    -- combinational functions
    X.Fact -> go01 $ \e -> Y.Call (Y.Function "jikka::notmod::fact" []) [e]
    X.Choose -> go02 $ \e1 e2 -> Y.Call (Y.Function "jikka::notmod::choose" []) [e1, e2]
    X.Permute -> go02 $ \e1 e2 -> Y.Call (Y.Function "jikka::notmod::permute" []) [e1, e2]
    X.MultiChoose -> go02 $ \e1 e2 -> Y.Call (Y.Function "jikka::notmod::multichoose" []) [e1, e2]
    -- data structures
    X.ConvexHullTrickInit -> go00 $ Y.Call Y.ConvexHullTrickCtor []
    X.ConvexHullTrickGetMin -> go02 $ \cht x -> Y.Call (Y.Method "get_min") [cht, x]
    X.ConvexHullTrickInsert -> go03 $ \cht a b -> Y.Call Y.ConvexHullTrickCopyAddLine [cht, a, b]
    X.SegmentTreeInitList semigrp -> go01 $ \a -> Y.Call (Y.SegmentTreeCtor (runSemigroup semigrp)) [a]
    X.SegmentTreeGetRange _ -> go03 $ \segtree l r -> Y.Call (Y.Method "prod") [segtree, l, r]
    X.SegmentTreeSetPoint semigrp -> go03 $ \segtree i a -> Y.Call (Y.SegmentTreeCopySetPoint (runSemigroup semigrp)) [segtree, i, a]

runExprFunction :: (MonadAlpha m, MonadError Error m) => Env -> X.Expr -> Y.Expr -> m ([Y.Statement], [Y.Statement], Y.Expr)
runExprFunction env f e = case f of
  X.Lam x t body -> do
    y <- renameVarName' Y.LocalArgumentNameKind x
    (stmts, body) <- runStatementsT $ runExpr ((x, t, y) : env) body
    let stmts' = map (Y.replaceStatement y e) stmts
    let body' = Y.replaceExpr y e body
    return ([], stmts', body')
  f -> do
    (stmts, f) <- runStatementsT $ runExpr env f
    return (stmts, [], Y.CallExpr f [e])

runExprFunction2 :: (MonadAlpha m, MonadError Error m) => Env -> X.Expr -> Y.Expr -> Y.Expr -> m ([Y.Statement], [Y.Statement], Y.Expr)
runExprFunction2 env f e1 e2 = case f of
  X.Lam2 x1 t1 x2 t2 body -> do
    y1 <- renameVarName' Y.LocalArgumentNameKind x1
    y2 <- renameVarName' Y.LocalArgumentNameKind x2
    (stmts, body) <- runStatementsT $ runExpr ((x2, t2, y2) : (x1, t1, y1) : env) body
    let stmts' = map (Y.replaceStatement y2 e2 . Y.replaceStatement y1 e1) stmts
    let body' = Y.replaceExpr y2 e2 $ Y.replaceExpr y1 e1 body
    return ([], stmts', body')
  f -> do
    (stmts, f) <- runStatementsT $ runExpr env f
    return (stmts, [], Y.CallExpr (Y.CallExpr f [e1]) [e2])

runExpr :: (MonadStatements m, MonadAlpha m, MonadError Error m) => Env -> X.Expr -> m Y.Expr
runExpr env = \case
  X.Var x -> do
    y <- lookupVarName env x
    return $ Y.Var y
  X.Lit lit -> do
    runLiteral env lit
  e@(X.App _ _) -> do
    let (f, args) = X.curryApp e
    case f of
      X.Lit (X.LitBuiltin builtin bts) -> do
        arity <- arityOfBuiltin builtin bts
        if length args < arity
          then do
            (ts, ret) <- X.uncurryFunTy <$> X.builtinToType builtin bts
            ts <- mapM runType ts
            ret <- runType ret
            xs <- replicateM (arity - length args) X.genVarName'
            ys <- mapM (renameVarName' Y.LocalArgumentNameKind) xs
            e <- runAppBuiltin env builtin bts (args ++ map X.Var xs)
            let (_, e') = foldr (\(t, y) (ret, e) -> (Y.TyFunction ret [t], Y.Lam [(t, y)] ret [Y.Return e])) (ret, e) (zip (drop (length args) ts) ys)
            return e'
          else
            if length args == arity
              then do
                runAppBuiltin env builtin bts args
              else do
                args' <- mapM (runExpr env) (drop arity args)
                e <- runAppBuiltin env builtin bts (take arity args)
                return $ Y.CallExpr e args'
      _ -> do
        f <- runExpr env f
        args <- mapM (runExpr env) args
        return $ Y.CallExpr f args
  e@(X.Lam _ _ _) -> do
    let (args, body) = X.uncurryLam e
    ys <- mapM (renameVarName' Y.LocalArgumentNameKind . fst) args
    let env' = reverse (zipWith (\(x, t) y -> (x, t, y)) args ys) ++ env
    ret <- runType =<< typecheckExpr env' body
    (stmts, body) <- runStatementsT $ runExpr env' body
    ts <- mapM (runType . snd) args
    let (_, [Y.Return e]) = foldr (\(t, y) (ret, body) -> (Y.TyFunction ret [t], [Y.Return (Y.Lam [(t, y)] ret body)])) (ret, stmts ++ [Y.Return body]) (zip ts ys)
    return e
  X.Let x t e1 e2 -> do
    y <- renameVarName' Y.LocalNameKind x
    t' <- runType t
    e1 <- runExpr env e1
    useStatement $ Y.Declare t' y (Y.DeclareCopy e1)
    runExpr ((x, t, y) : env) e2
  X.Assert e1 e2 -> do
    e1 <- runExpr env e1
    useStatement $ Y.Assert e1
    runExpr env e2

runToplevelFunDef :: (MonadAlpha m, MonadError Error m) => Env -> Y.VarName -> [(X.VarName, X.Type)] -> X.Type -> X.Expr -> m [Y.ToplevelStatement]
runToplevelFunDef env f args ret body = do
  ret <- runType ret
  args <- forM args $ \(x, t) -> do
    y <- renameVarName' Y.ArgumentNameKind x
    return (x, t, y)
  (stmts, result) <- runStatementsT $ runExpr (reverse args ++ env) body
  args <- forM args $ \(_, t, y) -> do
    t <- runType t
    return (t, y)
  return [Y.FunDef ret f args (stmts ++ [Y.Return result])]

runToplevelVarDef :: (MonadAlpha m, MonadError Error m) => Env -> Y.VarName -> X.Type -> X.Expr -> m [Y.ToplevelStatement]
runToplevelVarDef env x t e = do
  t <- runType t
  (stmts, e) <- runStatementsT $ runExpr env e
  case stmts of
    [] -> return [Y.VarDef t x e]
    _ -> return [Y.VarDef t x (Y.CallExpr (Y.Lam [] t (stmts ++ [Y.Return e])) [])]

runToplevelExpr :: (MonadAlpha m, MonadError Error m) => Env -> X.ToplevelExpr -> m [Y.ToplevelStatement]
runToplevelExpr env = \case
  X.ResultExpr e -> do
    t <- typecheckExpr env e
    case X.uncurryFunTy t of
      (ts@(_ : _), ret) -> do
        let f = Y.VarName "solve"
        (args, body) <- case X.uncurryLam e of
          (args, body) | length args == length ts -> do
            -- merge two sets of arguments which introduced by @FunTy@ and @Lam@
            args <- forM args $ \(x, t) -> do
              y <- renameVarName' Y.ArgumentNameKind x
              return (x, t, y)
            (stmts, e) <- runStatementsT $ runExpr (reverse args ++ env) body
            let body = stmts ++ [Y.Return e]
            args' <- forM args $ \(_, t, y) -> do
              t <- runType t
              return (t, y)
            return (args', body)
          _ -> do
            args <- forM ts $ \t -> do
              t <- runType t
              y <- Y.newFreshName Y.ArgumentNameKind
              return (t, y)
            (stmts, e) <- runStatementsT $ runExpr env e
            let body = stmts ++ [Y.Return (Y.CallExpr e (map (Y.Var . snd) args))]
            return (args, body)
        ret <- runType ret
        return [Y.FunDef ret f args body]
      _ -> throwInternalError "solve function must be a function" -- TODO: add check in restricted Python
  X.ToplevelLet x t e cont -> case (X.uncurryLam e, X.uncurryFunTy t) of
    ((args@(_ : _), body), (ts@(_ : _), ret)) -> do
      g <- renameVarName' Y.FunctionNameKind x
      (args, body) <-
        if length args < length ts
          then do
            xs <- replicateM (length ts - length args) X.genVarName'
            let args' = args ++ zip xs (drop (length args) ts)
            let body' = X.uncurryApp body (map X.Var xs)
            return (args', body')
          else return (args, body)
      stmt <- runToplevelFunDef ((x, t, g) : env) g args ret body
      cont <- runToplevelExpr ((x, t, g) : env) cont
      return $ stmt ++ cont
    _ -> do
      y <- renameVarName' Y.ConstantNameKind x
      stmt <- runToplevelVarDef env y t e
      cont <- runToplevelExpr ((x, t, y) : env) cont
      return $ stmt ++ cont
  X.ToplevelLetRec f args ret body cont -> do
    g <- renameVarName' Y.FunctionNameKind f
    let t = X.curryFunTy (map snd args) ret
    stmt <- runToplevelFunDef ((f, t, g) : env) g args ret body
    cont <- runToplevelExpr ((f, t, g) : env) cont
    return $ stmt ++ cont
  X.ToplevelAssert e cont -> do
    (stmts, e) <- runStatementsT $ runExpr env e
    let stmt = Y.StaticAssert (Y.CallExpr (Y.Lam [] Y.TyBool (stmts ++ [Y.Return e])) []) ""
    cont <- runToplevelExpr env cont
    return $ stmt : cont

runProgram :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
runProgram prog = Y.Program <$> runToplevelExpr [] prog

run :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
run prog = wrapError' "Jikka.CPlusPlus.Convert.FromCore" $ do
  runProgram prog
