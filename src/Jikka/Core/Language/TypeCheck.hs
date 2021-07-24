{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Language.RewriteRules
-- Description : checks and obtains types of exprs. / 式の型を検査し取得します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Language.TypeCheck where

import Jikka.Common.Error
import Jikka.Core.Format (formatExpr, formatType)
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Util

builtinToType :: Builtin -> Type
builtinToType = \case
  -- arithmetical functions
  Negate -> Fun1STy IntTy
  Plus -> Fun2STy IntTy
  Minus -> Fun2STy IntTy
  Mult -> Fun2STy IntTy
  FloorDiv -> Fun2STy IntTy
  FloorMod -> Fun2STy IntTy
  CeilDiv -> Fun2STy IntTy
  CeilMod -> Fun2STy IntTy
  Pow -> Fun2STy IntTy
  -- advanced arithmetical functions
  Abs -> Fun1STy IntTy
  Gcd -> Fun2STy IntTy
  Lcm -> Fun2STy IntTy
  Min2 t -> Fun2STy t
  Max2 t -> Fun2STy t
  Iterate t -> Fun3Ty IntTy (FunTy t t) t t
  -- logical functions
  Not -> Fun1STy BoolTy
  And -> Fun2STy BoolTy
  Or -> Fun2STy BoolTy
  Implies -> Fun2STy BoolTy
  If t -> Fun3Ty BoolTy t t t
  -- bitwise functions
  BitNot -> Fun1STy IntTy
  BitAnd -> Fun2STy IntTy
  BitOr -> Fun2STy IntTy
  BitXor -> Fun2STy IntTy
  BitLeftShift -> Fun2STy IntTy
  BitRightShift -> Fun2STy IntTy
  -- matrix functions
  MatAp h w -> Fun2Ty (matrixTy h w) (vectorTy w) (vectorTy h)
  MatZero n -> matrixTy n n
  MatOne n -> matrixTy n n
  MatAdd h w -> Fun2Ty (matrixTy h w) (matrixTy h w) (matrixTy h w)
  MatMul h n w -> Fun2Ty (matrixTy h n) (matrixTy n w) (matrixTy h w)
  MatPow n -> Fun2Ty (matrixTy n n) IntTy (matrixTy n n)
  VecFloorMod n -> Fun2Ty (vectorTy n) IntTy (vectorTy n)
  MatFloorMod h w -> Fun2Ty (matrixTy h w) IntTy (matrixTy h w)
  -- modular functions
  ModNegate -> Fun2STy IntTy
  ModPlus -> Fun3STy IntTy
  ModMinus -> Fun3STy IntTy
  ModMult -> Fun3STy IntTy
  ModInv -> Fun2STy IntTy
  ModPow -> Fun3STy IntTy
  ModMatAp h w -> Fun3Ty (matrixTy h w) (vectorTy w) IntTy (vectorTy h)
  ModMatAdd h w -> Fun3Ty (matrixTy h w) (matrixTy h w) IntTy (matrixTy h w)
  ModMatMul h n w -> Fun3Ty (matrixTy h n) (matrixTy n w) IntTy (matrixTy h w)
  ModMatPow n -> Fun3Ty (matrixTy n n) IntTy IntTy (matrixTy n n)
  -- list functions
  Cons t -> Fun2Ty t (ListTy t) (ListTy t)
  Snoc t -> Fun2Ty (ListTy t) t (ListTy t)
  Foldl t1 t2 -> Fun3Ty (Fun2Ty t2 t1 t2) t2 (ListTy t1) t2
  Scanl t1 t2 -> Fun3Ty (Fun2Ty t2 t1 t2) t2 (ListTy t1) (ListTy t2)
  Build t -> Fun3Ty (FunTy (ListTy t) t) (ListTy t) IntTy (ListTy t)
  Len t -> FunTy (ListTy t) IntTy
  Map t1 t2 -> Fun2Ty (FunTy t1 t2) (ListTy t1) (ListTy t2)
  Filter t -> Fun2Ty (FunTy t BoolTy) (ListTy t) (ListTy t)
  At t -> Fun2Ty (ListTy t) IntTy t
  SetAt t -> Fun3Ty (ListTy t) IntTy t (ListTy t)
  Elem t -> Fun2Ty t (ListTy t) BoolTy
  Sum -> FunLTy IntTy
  Product -> FunLTy IntTy
  ModSum -> Fun2Ty (ListTy IntTy) IntTy IntTy
  ModProduct -> Fun2Ty (ListTy IntTy) IntTy IntTy
  Min1 t -> FunLTy t
  Max1 t -> FunLTy t
  ArgMin t -> FunTy (ListTy t) IntTy
  ArgMax t -> FunTy (ListTy t) IntTy
  All -> FunLTy BoolTy
  Any -> FunLTy BoolTy
  Sorted t -> Fun1STy (ListTy t)
  Reversed t -> Fun1STy (ListTy t)
  Range1 -> FunTy IntTy (ListTy IntTy)
  Range2 -> Fun2Ty IntTy IntTy (ListTy IntTy)
  Range3 -> Fun3Ty IntTy IntTy IntTy (ListTy IntTy)
  -- tuple functions
  Tuple ts -> curryFunTy ts (TupleTy ts)
  Proj ts n -> FunTy (TupleTy ts) (ts !! n)
  -- comparison
  LessThan t -> Fun2Ty t t BoolTy
  LessEqual t -> Fun2Ty t t BoolTy
  GreaterThan t -> Fun2Ty t t BoolTy
  GreaterEqual t -> Fun2Ty t t BoolTy
  Equal t -> Fun2Ty t t BoolTy
  NotEqual t -> Fun2Ty t t BoolTy
  -- combinational functions
  Fact -> Fun1STy IntTy
  Choose -> Fun2STy IntTy
  Permute -> Fun2STy IntTy
  MultiChoose -> Fun2STy IntTy
  -- data structure
  ConvexHullTrickInit -> ConvexHullTrickTy
  ConvexHullTrickGetMin -> Fun2Ty ConvexHullTrickTy IntTy IntTy
  ConvexHullTrickInsert -> Fun3Ty ConvexHullTrickTy IntTy IntTy ConvexHullTrickTy
  SegmentTreeInitList semigrp -> FunTy (ListTy (semigroupToType semigrp)) (SegmentTreeTy semigrp)
  SegmentTreeGetRange semigrp -> Fun3Ty (SegmentTreeTy semigrp) IntTy IntTy (semigroupToType semigrp)
  SegmentTreeSetPoint semigrp -> Fun3Ty (SegmentTreeTy semigrp) IntTy (semigroupToType semigrp) (SegmentTreeTy semigrp)

semigroupToType :: Semigroup' -> Type
semigroupToType = \case
  SemigroupIntPlus -> IntTy
  SemigroupIntMin -> IntTy
  SemigroupIntMax -> IntTy

literalToType :: Literal -> Type
literalToType = \case
  LitBuiltin builtin -> builtinToType builtin
  LitInt _ -> IntTy
  LitBool _ -> BoolTy
  LitNil t -> ListTy t
  LitBottom t _ -> t

arityOfBuiltin :: Builtin -> Int
arityOfBuiltin = \case
  Min2 _ -> 2
  Max2 _ -> 2
  Foldl _ _ -> 3
  Iterate _ -> 3
  At _ -> 2
  Min1 _ -> 1
  Max1 _ -> 1
  Proj _ _ -> 1
  builtin -> length (fst (uncurryFunTy (builtinToType builtin)))

type TypeEnv = [(VarName, Type)]

-- | `typecheckExpr` checks that the given `Expr` has the correct types.
typecheckExpr :: MonadError Error m => TypeEnv -> Expr -> m Type
typecheckExpr env = \case
  Var x -> case lookup x env of
    Nothing -> throwInternalError $ "undefined variable: " ++ unVarName x
    Just t -> return t
  Lit lit -> return $ literalToType lit
  App f e -> do
    tf <- typecheckExpr env f
    te <- typecheckExpr env e
    case tf of
      FunTy te' ret | te' == te -> return ret
      _ -> throwInternalError $ "wrong type funcall: function = " ++ formatExpr f ++ " and argument = " ++ formatExpr e ++ ", function's type = " ++ formatType tf ++ ", but argument's type = " ++ formatType te
  Lam x t e -> FunTy t <$> typecheckExpr ((x, t) : env) e
  Let x t e1 e2 -> do
    t' <- typecheckExpr env e1
    when (t /= t') $ do
      throwInternalError $ "wrong type binding: " ++ formatExpr (Let x t e1 e2)
    typecheckExpr ((x, t) : env) e2

typecheckToplevelExpr :: MonadError Error m => TypeEnv -> ToplevelExpr -> m Type
typecheckToplevelExpr env = \case
  ResultExpr e -> typecheckExpr env e
  ToplevelLet x t e cont -> do
    t' <- typecheckExpr env e
    when (t' /= t) $ do
      throwInternalError $ "assigned type is not correct: context = (let " ++ unVarName x ++ ": " ++ formatType t ++ " = " ++ formatExpr e ++ " in ...), expected type = " ++ formatType t ++ ", actual type = " ++ formatType t'
    typecheckToplevelExpr ((x, t) : env) cont
  ToplevelLetRec f args ret body cont -> do
    let t = case args of
          [] -> ret
          _ -> curryFunTy (map snd args) ret
    ret' <- typecheckExpr (reverse args ++ (f, t) : env) body
    when (ret' /= ret) $ do
      throwInternalError $ "returned type is not correct: context = (let rec " ++ unVarName f ++ " " ++ unwords (map (\(x, t) -> unVarName x ++ ": " ++ formatType t) args) ++ ": " ++ formatType ret ++ " = " ++ formatExpr body ++ " in ...), expected type = " ++ formatType ret ++ ", actual type = " ++ formatType ret'
    typecheckToplevelExpr ((f, t) : env) cont

typecheckProgram :: MonadError Error m => Program -> m Type
typecheckProgram prog = wrapError' "Jikka.Core.Language.TypeCheck.typecheckProgram" $ do
  typecheckToplevelExpr [] prog
