{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Language.Core.Lint
-- Description : checks the invariants (e.g. types) of data types of our core language.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.Lint` module checks the invariants of data types. Mainly, this checks types of `Expr`.
module Jikka.Language.Core.Lint where

import Jikka.Language.Common.Name
import Jikka.Language.Core.Expr

fun1Ty :: Type -> Type
fun1Ty t = FunTy [t] t

-- | `fun2Ty` is a utility to define binary functions.
--
-- >>> fun2Ty IntTy
-- FunTy [IntTy,IntTy] IntTy
fun2Ty :: Type -> Type
fun2Ty t = FunTy [t, t] t

fun3Ty :: Type -> Type
fun3Ty t = FunTy [t, t, t] t

-- | `funlTy` is a utility to define unary functions from lists to elements.
--
-- >>> funlTy IntTy
-- FunTy [ListTy IntTy] IntTy
funlTy :: Type -> Type
funlTy t = FunTy [ListTy t] t

builtinToType :: Builtin -> Type
builtinToType = \case
  -- arithmetical functions
  Negate -> fun1Ty IntTy
  Plus -> fun2Ty IntTy
  Minus -> fun2Ty IntTy
  Mult -> fun2Ty IntTy
  FloorDiv -> fun2Ty IntTy
  FloorMod -> fun2Ty IntTy
  CeilDiv -> fun2Ty IntTy
  CeilMod -> fun2Ty IntTy
  Pow -> fun2Ty IntTy
  -- induction functions
  NatInd t -> FunTy [t, FunTy [IntTy, t] t] t
  -- advanced arithmetical functions
  Abs -> fun1Ty IntTy
  Gcd -> fun2Ty IntTy
  Lcm -> fun2Ty IntTy
  Min -> fun2Ty IntTy
  Max -> fun2Ty IntTy
  -- logical functions
  Not -> fun1Ty BoolTy
  And -> fun2Ty BoolTy
  Or -> fun2Ty BoolTy
  Implies -> fun2Ty BoolTy
  If t -> FunTy [BoolTy, t, t] t
  -- bitwise functions
  BitNot -> fun1Ty IntTy
  BitAnd -> fun2Ty IntTy
  BitOr -> fun2Ty IntTy
  BitXor -> fun2Ty IntTy
  BitLeftShift -> fun2Ty IntTy
  BitRightShift -> fun2Ty IntTy
  -- modular functions
  Inv -> fun2Ty IntTy
  PowMod -> fun3Ty IntTy
  -- list functions
  Len t -> FunTy [ListTy t] IntTy
  Tabulate t -> FunTy [IntTy, FunTy [IntTy] t] (ListTy t)
  Map t1 t2 -> FunTy [FunTy [t1] t2, ListTy t1] (ListTy t2)
  At t -> FunTy [ListTy t, IntTy] t
  Sum -> funlTy IntTy
  Product -> funlTy IntTy
  Min1 -> funlTy IntTy
  Max1 -> funlTy IntTy
  ArgMin -> funlTy IntTy
  ArgMax -> funlTy IntTy
  All -> funlTy BoolTy
  Any -> funlTy BoolTy
  Sorted t -> fun1Ty (ListTy t)
  List t -> fun1Ty (ListTy t)
  Reversed t -> fun1Ty (ListTy t)
  Range1 -> FunTy [IntTy] (ListTy IntTy)
  Range2 -> FunTy [IntTy, IntTy] (ListTy IntTy)
  Range3 -> FunTy [IntTy, IntTy, IntTy] (ListTy IntTy)
  -- arithmetical relations
  LessThan -> FunTy [IntTy, IntTy] BoolTy
  LessEqual -> FunTy [IntTy, IntTy] BoolTy
  GreaterThan -> FunTy [IntTy, IntTy] BoolTy
  GreaterEqual -> FunTy [IntTy, IntTy] BoolTy
  -- equality relations (polymorphic)
  Equal t -> FunTy [t, t] BoolTy
  NotEqual t -> FunTy [t, t] BoolTy
  -- combinational functions
  Fact -> fun1Ty IntTy
  Choose -> fun2Ty IntTy
  Permute -> fun2Ty IntTy
  MultiChoose -> fun2Ty IntTy

literalToType :: Literal -> Type
literalToType = \case
  LitBuiltin builtin -> builtinToType builtin
  LitInt _ -> IntTy
  LitBool _ -> BoolTy

type TypeEnv = [(VarName, Type)]

-- | `typecheckExpr` checks that the given `Expr` has the correct types.
typecheckExpr :: TypeEnv -> Expr -> Either String Type
typecheckExpr env = \case
  Var x -> case lookup x env of
    Nothing -> Left $ "Internal Error: undefined variable: " ++ show (unVarName x)
    Just t -> return t
  Lit lit -> return $ literalToType lit
  App e args -> do
    t <- typecheckExpr env e
    ts <- mapM (typecheckExpr env) args
    case t of
      FunTy ts' ret | ts' == ts -> return ret
      _ -> Left $ "Internal Error: invalid funcall: " ++ show (App e args, t, ts)
  Lam args e -> FunTy (map snd args) <$> typecheckExpr (reverse args ++ env) e
  Let x t e1 e2 -> do
    t' <- typecheckExpr env e1
    if t == t'
      then typecheckExpr ((x, t) : env) e2
      else Left $ "Internal Error: wrong type binding: " ++ show (Let x t e1 e2)

typecheckToplevelExpr :: TypeEnv -> ToplevelExpr -> Either String Type
typecheckToplevelExpr env = \case
  ResultExpr e -> typecheckExpr env e
  ToplevelLet rec x args ret body cont -> do
    let t = FunTy (map snd args) ret
    ret' <- case rec of
      NonRec -> typecheckExpr (reverse args ++ env) body
      Rec -> typecheckExpr (reverse args ++ (x, t) : env) body
    if ret' == ret then return () else Left "Internal Error: returned type is not corrent"
    typecheckToplevelExpr ((x, t) : env) cont

typecheckProgram :: Program -> Either String Type
typecheckProgram = typecheckToplevelExpr []

typecheckProgram' :: Program -> Either String Program
typecheckProgram' prog = do
  typecheckProgram prog
  return prog
