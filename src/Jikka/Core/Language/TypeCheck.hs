{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Language.TypeCheck where

import Jikka.Common.Error
import Jikka.Core.Convert.TypeInfer (literalToType)
import Jikka.Core.Language.Expr

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
  ToplevelLet x t e cont -> do
    t' <- typecheckExpr env e
    if t' == t then return () else throwInternalError "assigned type is not correct"
    typecheckToplevelExpr ((x, t) : env) cont
  ToplevelLetRec x args ret body cont -> do
    let t = case args of
          [] -> ret
          _ -> FunTy (map snd args) ret
    ret' <- typecheckExpr (reverse args ++ (x, t) : env) body
    if ret' == ret then return () else throwInternalError "returned type is not correct"
    typecheckToplevelExpr ((x, t) : env) cont

typecheckProgram :: MonadError Error m => Program -> m Type
typecheckProgram prog = wrapError' "Jikka.Core.Convert.TypeInfer.typecheckProgram" $ do
  typecheckToplevelExpr [] prog
