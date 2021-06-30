{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Language.TypeCheck where

import Data.List (intercalate)
import Jikka.Common.Error
import Jikka.Core.Format (formatExpr, formatType)
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
  NatInd t -> FunTy [t, FunTy [t] t, IntTy] t
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
  -- matrix functions
  MatAp h w -> FunTy [matrixTy h w, vectorTy w] (vectorTy h)
  MatZero n -> FunTy [] (matrixTy n n)
  MatOne n -> FunTy [] (matrixTy n n)
  MatAdd h w -> FunTy [matrixTy h w, matrixTy h w] (matrixTy h w)
  MatMul h n w -> FunTy [matrixTy h n, matrixTy n w] (matrixTy h w)
  MatPow n -> FunTy [matrixTy n n, IntTy] (matrixTy n n)
  -- modular functions
  ModInv -> Fun2Ty IntTy
  ModPow -> Fun3Ty IntTy
  ModMatAp h w -> FunTy [matrixTy h w, vectorTy w, IntTy] (vectorTy h)
  ModMatAdd h w -> FunTy [matrixTy h w, matrixTy h w, IntTy] (matrixTy h w)
  ModMatMul h n w -> FunTy [matrixTy h n, matrixTy n w, IntTy] (matrixTy h w)
  ModMatPow n -> FunTy [matrixTy n n, IntTy, IntTy] (matrixTy n n)
  -- list functions
  Cons t -> FunTy [t, ListTy t] (ListTy t)
  Foldl t1 t2 -> FunTy [FunTy [t2, t1] t2, t2, ListTy t1] t2
  Scanl t1 t2 -> FunTy [FunTy [t2, t1] t2, t2, ListTy t1] (ListTy t2)
  Len t -> FunTy [ListTy t] IntTy
  Tabulate t -> FunTy [IntTy, FunTy [IntTy] t] (ListTy t)
  Map t1 t2 -> FunTy [FunTy [t1] t2, ListTy t1] (ListTy t2)
  Filter t -> FunTy [FunTy [t] BoolTy, ListTy t] (ListTy t)
  At t -> FunTy [ListTy t, IntTy] t
  SetAt t -> FunTy [ListTy t, IntTy, t] (ListTy t)
  Elem t -> FunTy [t, ListTy t] BoolTy
  Sum -> FunLTy IntTy
  Product -> FunLTy IntTy
  ModProduct -> FunTy [ListTy IntTy, IntTy] IntTy
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
    Nothing -> throwInternalError $ "undefined variable: " ++ unVarName x
    Just t -> return t
  Lit lit -> return $ literalToType lit
  App e args -> do
    t <- typecheckExpr env e
    ts <- mapM (typecheckExpr env) args
    case t of
      FunTy ts' ret | ts' == ts -> return ret
      _ -> throwInternalError $ "wrong type funcall: expr = " ++ formatExpr (App e args) ++ ", expected type = " ++ intercalate " * " (map formatType ts) ++ " -> ?, actual type = " ++ formatType t
  Lam args e -> FunTy (map snd args) <$> typecheckExpr (reverse args ++ env) e
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
          _ -> FunTy (map snd args) ret
    ret' <- typecheckExpr (reverse args ++ (f, t) : env) body
    when (ret' /= ret) $ do
      throwInternalError $ "returned type is not correct: context = (let rec " ++ unVarName f ++ " " ++ unwords (map (\(x, t) -> unVarName x ++ ": " ++ formatType t) args) ++ ": " ++ formatType ret ++ " = " ++ formatExpr body ++ " in ...), expected type = " ++ formatType ret ++ ", actual type = " ++ formatType ret'
    typecheckToplevelExpr ((f, t) : env) cont

typecheckProgram :: MonadError Error m => Program -> m Type
typecheckProgram prog = wrapError' "Jikka.Core.Language.TypeCheck.typecheckProgram" $ do
  typecheckToplevelExpr [] prog
