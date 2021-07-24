{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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

import qualified Jikka.CPlusPlus.Language.Expr as Y
import qualified Jikka.CPlusPlus.Language.Util as Y
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Format as X (formatBuiltinIsolated, formatType)
import qualified Jikka.Core.Language.BuiltinPatterns as X
import qualified Jikka.Core.Language.Expr as X
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

runLiteral :: (MonadAlpha m, MonadError Error m) => Env -> X.Literal -> m Y.Expr
runLiteral env = \case
  X.LitBuiltin builtin -> do
    (stmts, e) <- runAppBuiltin env builtin []
    case stmts of
      [] -> return e
      _ -> throwInternalError "now builtin values don't use statements"
  X.LitInt n -> return $ Y.Lit (Y.LitInt64 n)
  X.LitBool p -> return $ Y.Lit (Y.LitBool p)
  X.LitNil t -> do
    t <- runType t
    return $ Y.Call (Y.Function "std::vector" [t]) []
  X.LitBottom t err -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::error" [t]) [Y.Lit (Y.LitString err)]

arityOfBuiltin :: X.Builtin -> Int
arityOfBuiltin = \case
  X.Min2 _ -> 2
  X.Max2 _ -> 2
  X.Foldl _ _ -> 3
  X.Iterate _ -> 3
  X.At _ -> 2
  X.Min1 _ -> 1
  X.Max1 _ -> 1
  X.Proj _ _ -> 1
  builtin -> length (fst (X.uncurryFunTy (X.builtinToType builtin)))

runAppBuiltin :: (MonadAlpha m, MonadError Error m) => Env -> X.Builtin -> [X.Expr] -> m ([Y.Statement], Y.Expr)
runAppBuiltin env f args = wrapError' ("converting builtin " ++ X.formatBuiltinIsolated f) $ do
  let go0 f = case args of
        [] -> return ([], f)
        _ -> throwInternalError $ "expected 0 arguments, got " ++ show (length args)
  let go1'' :: (MonadAlpha m, MonadError Error m) => (X.Expr -> m ([Y.Statement], Y.Expr)) -> m ([Y.Statement], Y.Expr)
      go1'' f = case args of
        [e1] -> f e1
        _ -> throwInternalError $ "expected 1 argument, got " ++ show (length args)
  let go1' :: (MonadAlpha m, MonadError Error m) => (Y.Expr -> m ([Y.Statement], Y.Expr)) -> m ([Y.Statement], Y.Expr)
      go1' f = go1'' $ \e1 -> do
        (stmts1, e1) <- runExpr env e1
        (stmts, e) <- f e1
        return (stmts1 ++ stmts, e)
  let go1 f = go1' (return . ([],) . f)
  let go2'' :: (MonadAlpha m, MonadError Error m) => (X.Expr -> X.Expr -> m ([Y.Statement], Y.Expr)) -> m ([Y.Statement], Y.Expr)
      go2'' f = case args of
        [e1, e2] -> f e1 e2
        _ -> throwInternalError $ "expected 2 arguments, got " ++ show (length args)
  let go2' :: (MonadAlpha m, MonadError Error m) => (Y.Expr -> Y.Expr -> m ([Y.Statement], Y.Expr)) -> m ([Y.Statement], Y.Expr)
      go2' f = go2'' $ \e1 e2 -> do
        (stmts1, e1) <- runExpr env e1
        (stmts2, e2) <- runExpr env e2
        (stmts, e) <- f e1 e2
        return (stmts1 ++ stmts2 ++ stmts, e)
  let go2 f = go2' (((return . ([],)) .) . f)
  let go3'' :: (MonadAlpha m, MonadError Error m) => (X.Expr -> X.Expr -> X.Expr -> m ([Y.Statement], Y.Expr)) -> m ([Y.Statement], Y.Expr)
      go3'' f = case args of
        [e1, e2, e3] -> f e1 e2 e3
        _ -> throwInternalError $ "expected 3 arguments, got " ++ show (length args)
  let go3' :: (MonadAlpha m, MonadError Error m) => (Y.Expr -> Y.Expr -> Y.Expr -> m ([Y.Statement], Y.Expr)) -> m ([Y.Statement], Y.Expr)
      go3' f = go3'' $ \e1 e2 e3 -> do
        (stmts1, e1) <- runExpr env e1
        (stmts2, e2) <- runExpr env e2
        (stmts3, e3) <- runExpr env e3
        (stmts, e) <- f e1 e2 e3
        return (stmts1 ++ stmts2 ++ stmts3 ++ stmts, e)
  let go3 f = go3' ((((return . ([],)) .) .) . f)
  let goN' :: (MonadAlpha m, MonadError Error m) => ([Y.Expr] -> m Y.Expr) -> m ([Y.Statement], Y.Expr)
      goN' f = do
        args <- mapM (runExpr env) args
        e <- f (map snd args)
        return (concatMap fst args, e)
  case f of
    -- arithmetical functions
    X.Negate -> go1 $ \e -> Y.UnOp Y.Negate e
    X.Plus -> go2 $ \e1 e2 -> Y.BinOp Y.Add e1 e2
    X.Minus -> go2 $ \e1 e2 -> Y.BinOp Y.Sub e1 e2
    X.Mult -> go2 $ \e1 e2 -> Y.BinOp Y.Mul e1 e2
    X.FloorDiv -> go2 $ \e1 e2 -> Y.Call (Y.Function "jikka::floordiv" []) [e1, e2]
    X.FloorMod -> go2 $ \e1 e2 -> Y.Call (Y.Function "jikka::floormod" []) [e1, e2]
    X.CeilDiv -> go2 $ \e1 e2 -> Y.Call (Y.Function "jikka::ceildiv" []) [e1, e2]
    X.CeilMod -> go2 $ \e1 e2 -> Y.Call (Y.Function "jikka::ceilmod" []) [e1, e2]
    X.Pow -> go2 $ \e1 e2 -> Y.Call (Y.Function "jikka::pow" []) [e1, e2]
    -- advanced arithmetical functions
    X.Abs -> go1 $ \e -> Y.Call (Y.Function "std::abs" []) [e]
    X.Gcd -> go2 $ \e1 e2 -> Y.Call (Y.Function "std::gcd" []) [e1, e2]
    X.Lcm -> go2 $ \e1 e2 -> Y.Call (Y.Function "std::lcm" []) [e1, e2]
    X.Min2 t -> go2' $ \e1 e2 -> do
      t <- runType t
      return ([], Y.Call (Y.Function "std::min" [t]) [e1, e2])
    X.Max2 t -> go2' $ \e1 e2 -> do
      t <- runType t
      return ([], Y.Call (Y.Function "std::max" [t]) [e1, e2])
    X.Iterate t -> go3'' $ \n f x -> do
      t <- runType t
      (stmtsN, n) <- runExpr env n
      (stmtsX, x) <- runExpr env x
      y <- Y.newFreshName Y.LocalNameKind
      i <- Y.newFreshName Y.LoopCounterNameKind
      (stmtsF, body, f) <- runExprFunction env f (Y.Var y)
      return
        ( stmtsN ++ stmtsX
            ++ [Y.Declare t y (Just x)]
            ++ stmtsF
            ++ [ Y.repStatement
                   i
                   (Y.cast Y.TyInt32 n)
                   (body ++ [Y.assignSimple y f])
               ],
          Y.Var y
        )
    -- logical functions
    X.Not -> go1 $ \e -> Y.UnOp Y.Not e
    X.And -> go2 $ \e1 e2 -> Y.BinOp Y.And e1 e2
    X.Or -> go2 $ \e1 e2 -> Y.BinOp Y.Or e1 e2
    X.Implies -> go2 $ \e1 e2 -> Y.BinOp Y.Or (Y.UnOp Y.Not e1) e2
    X.If t -> go3'' $ \e1 e2 e3 -> do
      (stmts1, e1') <- runExpr env e1
      (stmts2, e2') <- runExpr env e2
      (stmts3, e3') <- runExpr env e3
      case (stmts2, stmts3) of
        ([], [])
          | X.isConstantTimeExpr e2 && X.isConstantTimeExpr e3 ->
            return (stmts1, Y.Cond e1' e2' e3')
        _ -> do
          t <- runType t
          phi <- Y.newFreshName Y.LocalNameKind
          let assign = Y.Assign . Y.AssignExpr Y.SimpleAssign (Y.LeftVar phi)
          return ([Y.Declare t phi Nothing] ++ stmts1 ++ [Y.If e1' (stmts2 ++ [assign e2']) (Just (stmts3 ++ [assign e3']))], Y.Var phi)
    -- bitwise functions
    X.BitNot -> go1 $ \e -> Y.UnOp Y.BitNot e
    X.BitAnd -> go2 $ \e1 e2 -> Y.BinOp Y.BitAnd e1 e2
    X.BitOr -> go2 $ \e1 e2 -> Y.BinOp Y.BitOr e1 e2
    X.BitXor -> go2 $ \e1 e2 -> Y.BinOp Y.BitXor e1 e2
    X.BitLeftShift -> go2 $ \e1 e2 -> Y.BinOp Y.BitLeftShift e1 e2
    X.BitRightShift -> go2 $ \e1 e2 -> Y.BinOp Y.BitRightShift e1 e2
    -- matrix functions
    X.MatAp h w -> go2 $ \f x -> Y.Call (Y.Function "jikka::matap" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral w)]) [f, x]
    X.MatZero n -> go0 $ Y.Call (Y.Function "jikka::matzero" [Y.TyIntValue (fromIntegral n)]) []
    X.MatOne n -> go0 $ Y.Call (Y.Function "jikka::matone" [Y.TyIntValue (fromIntegral n)]) []
    X.MatAdd h w -> go2 $ \f g -> Y.Call (Y.Function "jikka::matadd" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral w)]) [f, g]
    X.MatMul h n w -> go2 $ \f g -> Y.Call (Y.Function "jikka::matmul" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral n), Y.TyIntValue (fromIntegral w)]) [f, g]
    X.MatPow n -> go2 $ \f k -> Y.Call (Y.Function "jikka::matpow" [Y.TyIntValue (fromIntegral n)]) [f, k]
    X.VecFloorMod n -> go2 $ \x m -> Y.Call (Y.Function "jikka::vecfloormod" [Y.TyIntValue (fromIntegral n)]) [x, m]
    X.MatFloorMod h w -> go2 $ \f m -> Y.Call (Y.Function "jikka::matfloormod" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral w)]) [f, m]
    -- modular functions
    X.ModNegate -> go2 $ \e1 e2 -> Y.Call (Y.Function "jikka::modnegate" []) [e1, e2]
    X.ModPlus -> go3 $ \e1 e2 e3 -> Y.Call (Y.Function "jikka::modplus" []) [e1, e2, e3]
    X.ModMinus -> go3 $ \e1 e2 e3 -> Y.Call (Y.Function "jikka::modminus" []) [e1, e2, e3]
    X.ModMult -> go3 $ \e1 e2 e3 -> Y.Call (Y.Function "jikka::modmult" []) [e1, e2, e3]
    X.ModInv -> go2 $ \e1 e2 -> Y.Call (Y.Function "jikka::modinv" []) [e1, e2]
    X.ModPow -> go3 $ \e1 e2 e3 -> Y.Call (Y.Function "jikka::modpow" []) [e1, e2, e3]
    X.ModMatAp h w -> go3 $ \f x m -> Y.Call (Y.Function "jikka::modmatap" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral w)]) [f, x, m]
    X.ModMatAdd h w -> go3 $ \f g m -> Y.Call (Y.Function "jikka::modmatadd" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral w)]) [f, g, m]
    X.ModMatMul h n w -> go3 $ \f g m -> Y.Call (Y.Function "jikka::modmatmul" [Y.TyIntValue (fromIntegral h), Y.TyIntValue (fromIntegral n), Y.TyIntValue (fromIntegral w)]) [f, g, m]
    X.ModMatPow n -> go3 $ \f k m -> Y.Call (Y.Function "jikka::modmatpow" [Y.TyIntValue (fromIntegral n)]) [f, k, m]
    -- list functions
    X.Cons t -> go2' $ \x xs -> do
      t <- runType t
      ys <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare (Y.TyVector t) ys Nothing,
            Y.callMethod' (Y.Var ys) "push_back" [x],
            Y.callMethod' (Y.Var ys) "insert" [Y.end (Y.Var ys), Y.begin xs, Y.end xs]
          ],
          Y.Var ys
        )
    X.Snoc t -> go2' $ \xs x -> do
      t <- runType t
      ys <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare (Y.TyVector t) ys (Just xs),
            Y.callMethod' (Y.Var ys) "push_back" [x]
          ],
          Y.Var ys
        )
    X.Foldl t1 t2 -> go3'' $ \f init xs -> do
      (stmtsInit, init) <- runExpr env init
      (stmtsXs, xs) <- runExpr env xs
      t1 <- runType t1
      t2 <- runType t2
      y <- Y.newFreshName Y.LocalNameKind
      x <- Y.newFreshName Y.LocalNameKind
      (stmtsF, body, f) <- runExprFunction2 env f (Y.Var y) (Y.Var x)
      return
        ( stmtsInit ++ stmtsXs
            ++ [Y.Declare t2 y (Just init)]
            ++ stmtsF
            ++ [ Y.ForEach
                   t1
                   x
                   xs
                   (body ++ [Y.assignSimple y f])
               ],
          Y.Var y
        )
    X.Scanl _ t2 -> go3'' $ \f init xs -> do
      (stmtsInit, init) <- runExpr env init
      (stmtsXs, xs) <- runExpr env xs
      t2 <- runType t2
      ys <- Y.newFreshName Y.LocalNameKind
      i <- Y.newFreshName Y.LoopCounterNameKind
      (stmtsF, body, f) <- runExprFunction2 env f (Y.at (Y.Var ys) (Y.Var i)) (Y.at xs (Y.Var i))
      return
        ( stmtsInit ++ stmtsXs
            ++ [ Y.Declare (Y.TyVector t2) ys (Just (Y.callFunction "std::vector" [t2] [Y.incrExpr (Y.size xs)])),
                 Y.assignAt ys (Y.litInt32 0) init
               ]
            ++ stmtsF
            ++ [ Y.repStatement
                   i
                   (Y.cast Y.TyInt32 (Y.size xs))
                   (body ++ [Y.assignAt ys (Y.incrExpr (Y.Var i)) f])
               ],
          Y.Var ys
        )
    X.Build t -> go3'' $ \f xs n -> do
      (stmtsInit, xs) <- runExpr env xs
      (stmtsXs, n) <- runExpr env n
      t <- runType t
      ys <- Y.newFreshName Y.LocalNameKind
      i <- Y.newFreshName Y.LoopCounterNameKind
      (stmtsF, body, f) <- runExprFunction env f (Y.Var ys)
      return
        ( stmtsInit ++ stmtsXs
            ++ [ Y.Declare (Y.TyVector t) ys (Just xs)
               ]
            ++ stmtsF
            ++ [ Y.repStatement
                   i
                   (Y.cast Y.TyInt32 n)
                   (body ++ [Y.callMethod' (Y.Var ys) "push_back" [f]])
               ],
          Y.Var ys
        )
    X.Len _ -> go1 $ \e -> Y.cast Y.TyInt64 (Y.size e)
    X.Map _ t2 -> go2'' $ \f xs -> do
      ys <- Y.newFreshName Y.LocalNameKind
      t2 <- runType t2
      stmts <- case (f, xs) of
        (X.Lam _ _ (X.Lit lit), X.Range1' n) -> do
          (stmtsN, n) <- runExpr env n
          lit <- runLiteral env lit
          return $
            stmtsN
              ++ [Y.Declare (Y.TyVector t2) ys (Just (Y.callFunction "std::vector" [t2] [n, lit]))]
        _ -> do
          (stmtsXs, xs) <- runExpr env xs
          i <- Y.newFreshName Y.LoopCounterNameKind
          (stmtsF, body, f) <- runExprFunction env f (Y.at xs (Y.Var i))
          return $
            stmtsXs
              ++ [Y.Declare (Y.TyVector t2) ys (Just (Y.callFunction "std::vector" [t2] [Y.size xs]))]
              ++ stmtsF
              ++ [ Y.repStatement
                     i
                     (Y.cast Y.TyInt32 (Y.size xs))
                     (body ++ [Y.assignAt ys (Y.Var i) f])
                 ]
      return (stmts, Y.Var ys)
    X.Filter t -> go2'' $ \f xs -> do
      (stmtsXs, xs) <- runExpr env xs
      t <- runType t
      ys <- Y.newFreshName Y.LocalNameKind
      x <- Y.newFreshName Y.LocalNameKind
      (stmtsF, body, f) <- runExprFunction env f (Y.Var x)
      return
        ( stmtsXs
            ++ [Y.Declare (Y.TyVector t) ys Nothing]
            ++ stmtsF
            ++ [ Y.ForEach
                   t
                   x
                   xs
                   ( body
                       ++ [ Y.If
                              f
                              [Y.callMethod' (Y.Var ys) "push_back" [Y.Var x]]
                              Nothing
                          ]
                   )
               ],
          Y.Var ys
        )
    X.At _ -> go2 $ \e1 e2 -> Y.at e1 e2
    X.SetAt t -> go3' $ \xs i x -> do
      t <- runType t
      ys <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare (Y.TyVector t) ys (Just xs),
            Y.assignAt ys i x
          ],
          Y.Var ys
        )
    X.Elem _ -> go2' $ \xs x -> do
      y <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare Y.TyBool y (Just (Y.BinOp Y.NotEqual (Y.callFunction "std::find" [] [Y.begin xs, Y.end xs, x]) (Y.end xs)))
          ],
          Y.Var y
        )
    X.Sum -> go1' $ \xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare Y.TyInt64 y (Just (Y.callFunction "std::accumulate" [] [Y.begin xs, Y.end xs, Y.litInt64 0]))
          ],
          Y.Var y
        )
    X.ModSum -> go2' $ \xs m -> do
      y <- Y.newFreshName Y.LocalNameKind
      x <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare Y.TyInt64 y (Just (Y.litInt64 0)),
            Y.ForEach
              Y.TyInt64
              x
              xs
              [Y.Assign (Y.AssignExpr Y.AddAssign (Y.LeftVar y) (Y.callFunction "jikka::floormod" [] [Y.Var x, m]))]
          ],
          Y.callFunction "jikka::floormod" [] [Y.Var y, m]
        )
    X.Product -> go1' $ \xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      x <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare Y.TyInt64 y (Just (Y.litInt64 1)),
            Y.ForEach
              Y.TyInt64
              x
              xs
              [Y.Assign (Y.AssignExpr Y.MulAssign (Y.LeftVar y) (Y.Var x))]
          ],
          Y.Var y
        )
    X.ModProduct -> go2' $ \xs m -> do
      y <- Y.newFreshName Y.LocalNameKind
      x <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare Y.TyInt64 y (Just (Y.litInt64 1)),
            Y.ForEach
              Y.TyInt64
              x
              xs
              [Y.Assign (Y.AssignExpr Y.SimpleAssign (Y.LeftVar y) (Y.callFunction "jikka::modmult" [] [Y.Var y, Y.Var x, m]))]
          ],
          Y.Var y
        )
    X.Min1 t -> go1' $ \xs -> do
      t <- runType t
      y <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare t y (Just (Y.UnOp Y.Deref (Y.callFunction "std::min_element" [] [Y.begin xs, Y.end xs])))
          ],
          Y.Var y
        )
    X.Max1 t -> go1' $ \xs -> do
      t <- runType t
      y <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare t y (Just (Y.UnOp Y.Deref (Y.callFunction "std::max_element" [] [Y.begin xs, Y.end xs])))
          ],
          Y.Var y
        )
    X.ArgMin t -> go1' $ \xs -> do
      t <- runType t
      y <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare t y (Just (Y.BinOp Y.Sub (Y.callFunction "std::min_element" [] [Y.begin xs, Y.end xs]) (Y.begin xs)))
          ],
          Y.Var y
        )
    X.ArgMax t -> go1' $ \xs -> do
      t <- runType t
      y <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare t y (Just (Y.BinOp Y.Sub (Y.callFunction "std::max_element" [] [Y.begin xs, Y.end xs]) (Y.begin xs)))
          ],
          Y.Var y
        )
    X.All -> go1' $ \xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare Y.TyBool y (Just (Y.BinOp Y.Equal (Y.callFunction "std::find" [] [Y.begin xs, Y.end xs, Y.Lit (Y.LitBool True)]) (Y.end xs)))
          ],
          Y.Var y
        )
    X.Any -> go1' $ \xs -> do
      y <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare Y.TyBool y (Just (Y.BinOp Y.NotEqual (Y.callFunction "std::find" [] [Y.begin xs, Y.end xs, Y.Lit (Y.LitBool False)]) (Y.end xs)))
          ],
          Y.Var y
        )
    X.Sorted t -> go1' $ \xs -> do
      t <- runType t
      ys <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare (Y.TyVector t) ys (Just xs),
            Y.callFunction' "std::sort" [] [Y.begin (Y.Var ys), Y.end (Y.Var ys)]
          ],
          Y.Var ys
        )
    X.Reversed t -> go1' $ \xs -> do
      t <- runType t
      ys <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare (Y.TyVector t) ys (Just xs),
            Y.callFunction' "std::reverse" [] [Y.begin (Y.Var ys), Y.end (Y.Var ys)]
          ],
          Y.Var ys
        )
    X.Range1 -> go1 $ \n -> Y.Call Y.Range [n]
    X.Range2 -> go2' $ \from to -> do
      ys <- Y.newFreshName Y.LocalNameKind
      return
        ( [ Y.Declare (Y.TyVector Y.TyInt64) ys (Just (Y.callFunction "std::vector" [Y.TyInt64] [Y.BinOp Y.Sub to from])),
            Y.callFunction' "std::iota" [] [Y.begin (Y.Var ys), Y.end (Y.Var ys), from]
          ],
          Y.Var ys
        )
    X.Range3 -> go3' $ \from to step -> do
      ys <- Y.newFreshName Y.LocalNameKind
      i <- Y.newFreshName Y.LoopCounterNameKind
      return
        ( [ Y.Declare (Y.TyVector Y.TyInt64) ys Nothing,
            Y.For
              Y.TyInt32
              i
              from
              (Y.BinOp Y.LessThan (Y.Var i) to)
              (Y.AssignExpr Y.AddAssign (Y.LeftVar i) step)
              [ Y.callMethod' (Y.Var ys) "push_back" [Y.Var i]
              ]
          ],
          Y.Var ys
        )
    -- tuple functions
    X.Tuple ts -> goN' $ \es -> do
      ts <- mapM runType ts
      return $
        if Y.shouldBeArray ts
          then Y.Call (Y.ArrayExt (head ts)) es
          else Y.Call (Y.StdTuple ts) es
    X.Proj ts n -> go1' $ \e -> do
      ts <- mapM runType ts
      return . ([],) $
        if Y.shouldBeArray ts
          then Y.at e (Y.Lit (Y.LitInt32 (fromIntegral n)))
          else Y.Call (Y.StdGet (toInteger n)) [e]
    -- comparison
    X.LessThan _ -> go2 $ \e1 e2 -> Y.BinOp Y.LessThan e1 e2
    X.LessEqual _ -> go2 $ \e1 e2 -> Y.BinOp Y.LessEqual e1 e2
    X.GreaterThan _ -> go2 $ \e1 e2 -> Y.BinOp Y.GreaterThan e1 e2
    X.GreaterEqual _ -> go2 $ \e1 e2 -> Y.BinOp Y.GreaterEqual e1 e2
    X.Equal _ -> go2 $ \e1 e2 -> Y.BinOp Y.Equal e1 e2
    X.NotEqual _ -> go2 $ \e1 e2 -> Y.BinOp Y.NotEqual e1 e2
    -- combinational functions
    X.Fact -> go1 $ \e -> Y.Call (Y.Function "jikka::fact" []) [e]
    X.Choose -> go2 $ \e1 e2 -> Y.Call (Y.Function "jikka::choose" []) [e1, e2]
    X.Permute -> go2 $ \e1 e2 -> Y.Call (Y.Function "jikka::permute" []) [e1, e2]
    X.MultiChoose -> go2 $ \e1 e2 -> Y.Call (Y.Function "jikka::multichoose" []) [e1, e2]
    -- data structures
    X.ConvexHullTrickInit -> go0 $ Y.Call Y.ConvexHullTrickMake []
    X.ConvexHullTrickGetMin -> go2 $ \cht x -> Y.Call (Y.Method "get_min") [cht, x]
    X.ConvexHullTrickInsert -> go3 $ \cht a b -> Y.Call Y.ConvexHullTrickCopyAddLine [cht, a, b]
    X.SegmentTreeInitList semigrp -> go1 $ \a -> Y.Call (Y.SegmentTreeMake (runSemigroup semigrp)) [a]
    X.SegmentTreeGetRange _ -> go3 $ \segtree l r -> Y.Call (Y.Method "prod") [segtree, l, r]
    X.SegmentTreeSetPoint semigrp -> go3 $ \segtree i a -> Y.Call (Y.SegmentTreeCopySetPoint (runSemigroup semigrp)) [segtree, i, a]

runExprFunction :: (MonadAlpha m, MonadError Error m) => Env -> X.Expr -> Y.Expr -> m ([Y.Statement], [Y.Statement], Y.Expr)
runExprFunction env f e = case f of
  X.Lam x t body -> do
    y <- renameVarName' Y.LocalArgumentNameKind x
    (stmts, body) <- runExpr ((x, t, y) : env) body
    let stmts' = map (Y.replaceStatement y e) stmts
    let body' = Y.replaceExpr y e body
    return ([], stmts', body')
  f -> do
    (stmts, f) <- runExpr env f
    return (stmts, [], Y.CallExpr f [e])

runExprFunction2 :: (MonadAlpha m, MonadError Error m) => Env -> X.Expr -> Y.Expr -> Y.Expr -> m ([Y.Statement], [Y.Statement], Y.Expr)
runExprFunction2 env f e1 e2 = case f of
  X.Lam2 x1 t1 x2 t2 body -> do
    y1 <- renameVarName' Y.LocalArgumentNameKind x1
    y2 <- renameVarName' Y.LocalArgumentNameKind x2
    (stmts, body) <- runExpr ((x2, t2, y2) : (x1, t1, y1) : env) body
    let stmts' = map (Y.replaceStatement y2 e2 . Y.replaceStatement y1 e1) stmts
    let body' = Y.replaceExpr y2 e2 $ Y.replaceExpr y1 e1 body
    return ([], stmts', body')
  f -> do
    (stmts, f) <- runExpr env f
    return (stmts, [], Y.CallExpr (Y.CallExpr f [e1]) [e2])

runExpr :: (MonadAlpha m, MonadError Error m) => Env -> X.Expr -> m ([Y.Statement], Y.Expr)
runExpr env = \case
  X.Var x -> do
    y <- lookupVarName env x
    return ([], Y.Var y)
  X.Lit lit -> do
    lit <- runLiteral env lit
    return ([], lit)
  e@(X.App _ _) -> do
    let (f, args) = X.curryApp e
    case f of
      X.Lit (X.LitBuiltin builtin) -> do
        let arity = arityOfBuiltin builtin
        if length args < arity
          then do
            let (ts, ret) = X.uncurryFunTy (X.builtinToType builtin)
            ts <- mapM runType ts
            ret <- runType ret
            xs <- replicateM (arity - length args) X.genVarName'
            ys <- mapM (renameVarName' Y.LocalArgumentNameKind) xs
            (stmts, e) <- runAppBuiltin env builtin (args ++ map X.Var xs)
            let (_, e') = foldr (\(t, y) (ret, e) -> (Y.TyFunction ret [t], Y.Lam [(t, y)] ret [Y.Return e])) (ret, e) (zip (drop (length args) ts) ys)
            return (stmts, e')
          else
            if length args == arity
              then do
                runAppBuiltin env builtin args
              else do
                (stmts, e) <- runAppBuiltin env builtin (take arity args)
                args <- mapM (runExpr env) (drop arity args)
                return (concatMap fst args ++ stmts, Y.CallExpr e (map snd args))
      _ -> do
        args <- mapM (runExpr env) args
        (stmts, f) <- runExpr env f
        return (stmts ++ concatMap fst args, Y.CallExpr f (map snd args))
  e@(X.Lam _ _ _) -> do
    let (args, body) = X.uncurryLam e
    ys <- mapM (renameVarName' Y.LocalArgumentNameKind . fst) args
    let env' = reverse (zipWith (\(x, t) y -> (x, t, y)) args ys) ++ env
    ret <- runType =<< typecheckExpr env' body
    (stmts, body) <- runExpr env' body
    ts <- mapM (runType . snd) args
    let (_, [Y.Return e]) = foldr (\(t, y) (ret, body) -> (Y.TyFunction ret [t], [Y.Return (Y.Lam [(t, y)] ret body)])) (ret, stmts ++ [Y.Return body]) (zip ts ys)
    return ([], e)
  X.Let x t e1 e2 -> do
    y <- renameVarName' Y.LocalNameKind x
    t' <- runType t
    (stmts1, e1) <- runExpr env e1
    (stmts2, e2) <- runExpr ((x, t, y) : env) e2
    return (stmts1 ++ Y.Declare t' y (Just e1) : stmts2, e2)

runToplevelFunDef :: (MonadAlpha m, MonadError Error m) => Env -> Y.VarName -> [(X.VarName, X.Type)] -> X.Type -> X.Expr -> m [Y.ToplevelStatement]
runToplevelFunDef env f args ret body = do
  ret <- runType ret
  args <- forM args $ \(x, t) -> do
    y <- renameVarName' Y.ArgumentNameKind x
    return (x, t, y)
  (stmts, result) <- runExpr (reverse args ++ env) body
  args <- forM args $ \(_, t, y) -> do
    t <- runType t
    return (t, y)
  return [Y.FunDef ret f args (stmts ++ [Y.Return result])]

runToplevelVarDef :: (MonadAlpha m, MonadError Error m) => Env -> Y.VarName -> X.Type -> X.Expr -> m [Y.ToplevelStatement]
runToplevelVarDef env x t e = do
  t <- runType t
  (stmts, e) <- runExpr env e
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
            (stmts, e) <- runExpr (reverse args ++ env) body
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
            (stmts, e) <- runExpr env e
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

runProgram :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
runProgram prog = Y.Program <$> runToplevelExpr [] prog

run :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
run prog = wrapError' "Jikka.CPlusPlus.Convert.FromCore" $ do
  runProgram prog
