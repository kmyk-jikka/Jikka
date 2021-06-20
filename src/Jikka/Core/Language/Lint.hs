{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Language.Lint
-- Description : checks the invariants (e.g. types) of data types of our core language.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Core.Language.Lint` module checks the invariants of data types. Mainly, this checks types of `Expr`.
module Jikka.Core.Language.Lint where

import Jikka.Common.Error
import Jikka.Core.Language.Expr

builtinToType :: Builtin -> Type
builtinToType = \case
  -- arithmetical functions
  Negate -> Fun1Ty IntTy
  Plus -> Fun2Ty IntTy
  Minus -> Fun2Ty IntTy
  Mult -> Fun2Ty IntTy
  FloorDiv -> Fun2Ty IntTy
  FloorMod -> Fun2Ty IntTy
  CeilDiv -> Fun2Ty IntTy
  CeilMod -> Fun2Ty IntTy
  Pow -> Fun2Ty IntTy
  -- induction functions
  NatInd t -> FunTy [t, FunTy [IntTy, t] t, IntTy] t
  -- advanced arithmetical functions
  Abs -> Fun1Ty IntTy
  Gcd -> Fun2Ty IntTy
  Lcm -> Fun2Ty IntTy
  Min2 t -> Fun2Ty t
  Max2 t -> Fun2Ty t
  -- logical functions
  Not -> Fun1Ty BoolTy
  And -> Fun2Ty BoolTy
  Or -> Fun2Ty BoolTy
  Implies -> Fun2Ty BoolTy
  If t -> FunTy [BoolTy, t, t] t
  -- bitwise functions
  BitNot -> Fun1Ty IntTy
  BitAnd -> Fun2Ty IntTy
  BitOr -> Fun2Ty IntTy
  BitXor -> Fun2Ty IntTy
  BitLeftShift -> Fun2Ty IntTy
  BitRightShift -> Fun2Ty IntTy
  -- modular functions
  ModInv -> Fun2Ty IntTy
  ModPow -> Fun3Ty IntTy
  -- list functions
  Cons t -> FunTy [t, ListTy t] (ListTy t)
  Len t -> FunTy [ListTy t] IntTy
  Tabulate t -> FunTy [IntTy, FunTy [IntTy] t] (ListTy t)
  Map t1 t2 -> FunTy [FunTy [t1] t2, ListTy t1] (ListTy t2)
  Filter t -> FunTy [FunTy [t] BoolTy, ListTy t] (ListTy t)
  At t -> FunTy [ListTy t, IntTy] t
  Elem t -> FunTy [t, ListTy t] BoolTy
  Sum -> FunLTy IntTy
  Product -> FunLTy IntTy
  Min1 t -> FunLTy t
  Max1 t -> FunLTy t
  ArgMin t -> FunTy [ListTy t] IntTy
  ArgMax t -> FunTy [ListTy t] IntTy
  All -> FunLTy BoolTy
  Any -> FunLTy BoolTy
  Sorted t -> Fun1Ty (ListTy t)
  List t -> Fun1Ty (ListTy t)
  Reversed t -> Fun1Ty (ListTy t)
  Range1 -> FunTy [IntTy] (ListTy IntTy)
  Range2 -> FunTy [IntTy, IntTy] (ListTy IntTy)
  Range3 -> FunTy [IntTy, IntTy, IntTy] (ListTy IntTy)
  -- tuple functions
  Tuple ts -> FunTy ts (TupleTy ts)
  Proj ts n -> FunTy [TupleTy ts] (ts !! n)
  -- comparison
  LessThan t -> FunTy [t, t] BoolTy
  LessEqual t -> FunTy [t, t] BoolTy
  GreaterThan t -> FunTy [t, t] BoolTy
  GreaterEqual t -> FunTy [t, t] BoolTy
  Equal t -> FunTy [t, t] BoolTy
  NotEqual t -> FunTy [t, t] BoolTy
  -- combinational functions
  Fact -> Fun1Ty IntTy
  Choose -> Fun2Ty IntTy
  Permute -> Fun2Ty IntTy
  MultiChoose -> Fun2Ty IntTy

literalToType :: Literal -> Type
literalToType = \case
  LitBuiltin builtin -> builtinToType builtin
  LitInt _ -> IntTy
  LitBool _ -> BoolTy
  LitNil t -> ListTy t

type TypeEnv = [(VarName, Type)]

-- | `typecheckExpr` checks that the given `Expr` has the correct types.
typecheckExpr :: MonadError Error m => TypeEnv -> Expr -> m Type
typecheckExpr env = \case
  Var x -> case lookup x env of
    Nothing -> throwInternalError $ "undefined variable: " ++ show (unVarName x)
    Just t -> return t
  Lit lit -> return $ literalToType lit
  App e args -> do
    t <- typecheckExpr env e
    ts <- mapM (typecheckExpr env) args
    case t of
      FunTy ts' ret | ts' == ts -> return ret
      _ -> throwInternalError $ "invalid funcall: " ++ show (App e args, t, ts)
  Lam args e -> FunTy (map snd args) <$> typecheckExpr (reverse args ++ env) e
  Let x t e1 e2 -> do
    t' <- typecheckExpr env e1
    if t == t'
      then typecheckExpr ((x, t) : env) e2
      else throwInternalError $ "wrong type binding: " ++ show (Let x t e1 e2)

typecheckToplevelExpr :: MonadError Error m => TypeEnv -> ToplevelExpr -> m Type
typecheckToplevelExpr env = \case
  ResultExpr e -> typecheckExpr env e
  ToplevelLet rec x args ret body cont -> do
    let t = case args of
          [] -> ret
          _ -> FunTy (map snd args) ret
    ret' <- case rec of
      NonRec -> typecheckExpr (reverse args ++ env) body
      Rec -> typecheckExpr (reverse args ++ (x, t) : env) body
    if ret' == ret then return () else throwInternalError "returned type is not corrent"
    typecheckToplevelExpr ((x, t) : env) cont

typecheckProgram :: MonadError Error m => Program -> m Type
typecheckProgram = typecheckToplevelExpr []

typecheckProgram' :: MonadError Error m => Program -> m Program
typecheckProgram' prog = do
  typecheckProgram prog
  return prog
