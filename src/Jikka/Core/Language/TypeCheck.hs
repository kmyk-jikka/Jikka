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
import Jikka.Core.Format (formatBuiltinIsolated, formatExpr, formatType)
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Util

builtinToType :: MonadError Error m => Builtin -> [Type] -> m Type
builtinToType builtin ts =
  let go0 f = return f
      go1 f = case ts of
        [t1] -> return $ f t1
        _ -> throwInternalError $ "expected 1 type argument, but got " ++ show (length ts) ++ ": " ++ formatBuiltinIsolated builtin ts
      go2 f = case ts of
        [t1, t2] -> return $ f t1 t2
        _ -> throwInternalError $ "expected 2 type arguments, but got " ++ show (length ts) ++ ": " ++ formatBuiltinIsolated builtin ts
   in case builtin of
        -- arithmetical functions
        Negate -> go0 $ Fun1STy IntTy
        Plus -> go0 $ Fun2STy IntTy
        Minus -> go0 $ Fun2STy IntTy
        Mult -> go0 $ Fun2STy IntTy
        FloorDiv -> go0 $ Fun2STy IntTy
        FloorMod -> go0 $ Fun2STy IntTy
        CeilDiv -> go0 $ Fun2STy IntTy
        CeilMod -> go0 $ Fun2STy IntTy
        JustDiv -> go0 $ Fun2STy IntTy
        Pow -> go0 $ Fun2STy IntTy
        -- advanced arithmetical functions
        Abs -> go0 $ Fun1STy IntTy
        Gcd -> go0 $ Fun2STy IntTy
        Lcm -> go0 $ Fun2STy IntTy
        Min2 -> go1 $ \t -> Fun2STy t
        Max2 -> go1 $ \t -> Fun2STy t
        Iterate -> go1 $ \t -> Fun3Ty IntTy (FunTy t t) t t
        -- logical functions
        Not -> go0 $ Fun1STy BoolTy
        And -> go0 $ Fun2STy BoolTy
        Or -> go0 $ Fun2STy BoolTy
        Implies -> go0 $ Fun2STy BoolTy
        If -> go1 $ \t -> Fun3Ty BoolTy t t t
        -- bitwise functions
        BitNot -> go0 $ Fun1STy IntTy
        BitAnd -> go0 $ Fun2STy IntTy
        BitOr -> go0 $ Fun2STy IntTy
        BitXor -> go0 $ Fun2STy IntTy
        BitLeftShift -> go0 $ Fun2STy IntTy
        BitRightShift -> go0 $ Fun2STy IntTy
        -- matrix functions
        MatAp h w -> go0 $ Fun2Ty (matrixTy h w) (vectorTy w) (vectorTy h)
        MatZero h w -> go0 $ matrixTy h w
        MatOne n -> go0 $ matrixTy n n
        MatAdd h w -> go0 $ Fun2Ty (matrixTy h w) (matrixTy h w) (matrixTy h w)
        MatMul h n w -> go0 $ Fun2Ty (matrixTy h n) (matrixTy n w) (matrixTy h w)
        MatPow n -> go0 $ Fun2Ty (matrixTy n n) IntTy (matrixTy n n)
        VecFloorMod n -> go0 $ Fun2Ty (vectorTy n) IntTy (vectorTy n)
        MatFloorMod h w -> go0 $ Fun2Ty (matrixTy h w) IntTy (matrixTy h w)
        -- modular functions
        ModNegate -> go0 $ Fun2STy IntTy
        ModPlus -> go0 $ Fun3STy IntTy
        ModMinus -> go0 $ Fun3STy IntTy
        ModMult -> go0 $ Fun3STy IntTy
        ModInv -> go0 $ Fun2STy IntTy
        ModPow -> go0 $ Fun3STy IntTy
        ModMatAp h w -> go0 $ Fun3Ty (matrixTy h w) (vectorTy w) IntTy (vectorTy h)
        ModMatAdd h w -> go0 $ Fun3Ty (matrixTy h w) (matrixTy h w) IntTy (matrixTy h w)
        ModMatMul h n w -> go0 $ Fun3Ty (matrixTy h n) (matrixTy n w) IntTy (matrixTy h w)
        ModMatPow n -> go0 $ Fun3Ty (matrixTy n n) IntTy IntTy (matrixTy n n)
        -- list functions
        Cons -> go1 $ \t -> Fun2Ty t (ListTy t) (ListTy t)
        Snoc -> go1 $ \t -> Fun2Ty (ListTy t) t (ListTy t)
        Foldl -> go2 $ \t1 t2 -> Fun3Ty (Fun2Ty t2 t1 t2) t2 (ListTy t1) t2
        Scanl -> go2 $ \t1 t2 -> Fun3Ty (Fun2Ty t2 t1 t2) t2 (ListTy t1) (ListTy t2)
        Build -> go1 $ \t -> Fun3Ty (FunTy (ListTy t) t) (ListTy t) IntTy (ListTy t)
        Len -> go1 $ \t -> FunTy (ListTy t) IntTy
        Map -> go2 $ \t1 t2 -> Fun2Ty (FunTy t1 t2) (ListTy t1) (ListTy t2)
        Filter -> go1 $ \t -> Fun2Ty (FunTy t BoolTy) (ListTy t) (ListTy t)
        At -> go1 $ \t -> Fun2Ty (ListTy t) IntTy t
        SetAt -> go1 $ \t -> Fun3Ty (ListTy t) IntTy t (ListTy t)
        Elem -> go1 $ \t -> Fun2Ty t (ListTy t) BoolTy
        Sum -> go0 $ FunLTy IntTy
        Product -> go0 $ FunLTy IntTy
        ModSum -> go0 $ Fun2Ty (ListTy IntTy) IntTy IntTy
        ModProduct -> go0 $ Fun2Ty (ListTy IntTy) IntTy IntTy
        Min1 -> go1 $ \t -> FunLTy t
        Max1 -> go1 $ \t -> FunLTy t
        ArgMin -> go1 $ \t -> FunTy (ListTy t) IntTy
        ArgMax -> go1 $ \t -> FunTy (ListTy t) IntTy
        Gcd1 -> go0 $ FunLTy IntTy
        Lcm1 -> go0 $ FunLTy IntTy
        All -> go0 $ FunLTy BoolTy
        Any -> go0 $ FunLTy BoolTy
        Sorted -> go1 $ \t -> Fun1STy (ListTy t)
        Reversed -> go1 $ \t -> Fun1STy (ListTy t)
        Range1 -> go0 $ FunTy IntTy (ListTy IntTy)
        Range2 -> go0 $ Fun2Ty IntTy IntTy (ListTy IntTy)
        Range3 -> go0 $ Fun3Ty IntTy IntTy IntTy (ListTy IntTy)
        -- tuple functions
        Tuple -> return $ curryFunTy ts (TupleTy ts)
        Proj n ->
          if 0 <= n && n < toInteger (length ts)
            then return $ FunTy (TupleTy ts) (ts !! fromInteger n)
            else throwTypeError $ "projection index is out of range: type = " ++ formatType (TupleTy ts) ++ ", index = " ++ show n
        -- comparison
        LessThan -> go1 $ \t -> Fun2Ty t t BoolTy
        LessEqual -> go1 $ \t -> Fun2Ty t t BoolTy
        GreaterThan -> go1 $ \t -> Fun2Ty t t BoolTy
        GreaterEqual -> go1 $ \t -> Fun2Ty t t BoolTy
        Equal -> go1 $ \t -> Fun2Ty t t BoolTy
        NotEqual -> go1 $ \t -> Fun2Ty t t BoolTy
        -- combinational functions
        Fact -> go0 $ Fun1STy IntTy
        Choose -> go0 $ Fun2STy IntTy
        Permute -> go0 $ Fun2STy IntTy
        MultiChoose -> go0 $ Fun2STy IntTy
        -- data structure
        ConvexHullTrickInit -> go0 ConvexHullTrickTy
        ConvexHullTrickGetMin -> go0 $ Fun2Ty ConvexHullTrickTy IntTy IntTy
        ConvexHullTrickInsert -> go0 $ Fun3Ty ConvexHullTrickTy IntTy IntTy ConvexHullTrickTy
        SegmentTreeInitList semigrp -> go0 $ FunTy (ListTy (semigroupToType semigrp)) (SegmentTreeTy semigrp)
        SegmentTreeGetRange semigrp -> go0 $ Fun3Ty (SegmentTreeTy semigrp) IntTy IntTy (semigroupToType semigrp)
        SegmentTreeSetPoint semigrp -> go0 $ Fun3Ty (SegmentTreeTy semigrp) IntTy (semigroupToType semigrp) (SegmentTreeTy semigrp)

semigroupToType :: Semigroup' -> Type
semigroupToType = \case
  SemigroupIntPlus -> IntTy
  SemigroupIntMin -> IntTy
  SemigroupIntMax -> IntTy
  SemigroupIntGcd -> IntTy
  SemigroupIntLcm -> IntTy

literalToType :: MonadError Error m => Literal -> m Type
literalToType = \case
  LitBuiltin builtin ts -> builtinToType builtin ts
  LitInt _ -> return IntTy
  LitBool _ -> return BoolTy
  LitNil t -> return $ ListTy t
  LitBottom t _ -> return t

arityOfBuiltin :: MonadError Error m => Builtin -> [Type] -> m Int
arityOfBuiltin builtin ts = case builtin of
  Min2 -> return 2
  Max2 -> return 2
  Foldl -> return 3
  Iterate -> return 3
  At -> return 2
  Min1 -> return 1
  Max1 -> return 1
  Proj _ -> return 1
  builtin -> length . fst . uncurryFunTy <$> builtinToType builtin ts

type TypeEnv = [(VarName, Type)]

-- | `typecheckExpr` checks that the given `Expr` has the correct types.
typecheckExpr :: MonadError Error m => TypeEnv -> Expr -> m Type
typecheckExpr env = \case
  Var x -> case lookup x env of
    Nothing -> throwInternalError $ "undefined variable: " ++ unVarName x
    Just t -> return t
  Lit lit -> literalToType lit
  App f e -> do
    tf <- typecheckExpr env f
    te <- typecheckExpr env e
    case tf of
      FunTy te' ret | te' == te -> return ret
      _ -> throwInternalError $ "wrong type funcall: function = " ++ formatExpr f ++ " and argument = " ++ formatExpr e ++ ", function's type = " ++ formatType tf ++ ", but argument's type = " ++ formatType te
  Lam x t e ->
    let env' = if x == VarName "_" then env else (x, t) : env
     in FunTy t <$> typecheckExpr env' e
  Let x t e1 e2 -> do
    t' <- typecheckExpr env e1
    when (t /= t') $ do
      throwInternalError $ "wrong type binding: " ++ formatExpr (Let x t e1 e2)
    let env' = if x == VarName "_" then env else (x, t) : env
    typecheckExpr env' e2
  Assert e1 e2 -> do
    t <- typecheckExpr env e1
    when (t /= BoolTy) $ do
      throwInternalError $ "wrong type assertion: expr = " ++ formatExpr e1 ++ " has type = " ++ formatType t
    typecheckExpr env e2

typecheckToplevelExpr :: MonadError Error m => TypeEnv -> ToplevelExpr -> m Type
typecheckToplevelExpr env = \case
  ResultExpr e -> typecheckExpr env e
  ToplevelLet x t e cont -> do
    t' <- typecheckExpr env e
    when (t' /= t) $ do
      throwInternalError $ "assigned type is not correct: context = (let " ++ unVarName x ++ ": " ++ formatType t ++ " = " ++ formatExpr e ++ " in ...), expected type = " ++ formatType t ++ ", actual type = " ++ formatType t'
    typecheckToplevelExpr ((x, t) : env) cont
  ToplevelLetRec f args ret body cont -> do
    let t = curryFunTy (map snd args) ret
    ret' <- typecheckExpr (reverse args ++ (f, t) : env) body
    when (ret' /= ret) $ do
      throwInternalError $ "returned type is not correct: context = (let rec " ++ unVarName f ++ " " ++ unwords (map (\(x, t) -> unVarName x ++ ": " ++ formatType t) args) ++ ": " ++ formatType ret ++ " = " ++ formatExpr body ++ " in ...), expected type = " ++ formatType ret ++ ", actual type = " ++ formatType ret'
    typecheckToplevelExpr ((f, t) : env) cont
  ToplevelAssert e1 e2 -> do
    t <- typecheckExpr env e1
    when (t /= BoolTy) $ do
      throwInternalError $ "wrong type toplevel assertion: expr = " ++ formatExpr e1 ++ " has type = " ++ formatType t
    typecheckToplevelExpr env e2

typecheckProgram :: MonadError Error m => Program -> m Type
typecheckProgram prog = wrapError' "Jikka.Core.Language.TypeCheck.typecheckProgram" $ do
  typecheckToplevelExpr [] prog
