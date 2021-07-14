{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Jikka.RestrictedPython.Convert.DefaultMain
-- Description : makes a default IO format based on types. / 型に基づくデフォルトの入出力フォーマットを作成します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Convert.DefaultMain
  ( run,
  )
where

import Control.Arrow
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.IOFormat
import Jikka.RestrictedPython.Format (formatType)
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util

lookupSolve :: MonadError Error m => Program -> m (Maybe Loc, [(VarName', Type)], Type, [Statement])
lookupSolve = \case
  [] -> throwSymbolError "solve function is not defined"
  ToplevelAnnAssign _ _ _ : stmts -> lookupSolve stmts
  ToplevelFunctionDef f args ret body : stmts -> case value' f of
    VarName "solve" -> return (loc' f, args, ret, body)
    _ -> lookupSolve stmts
  ToplevelAssert _ : stmts -> lookupSolve stmts

makeInputFormatFromType :: (MonadAlpha m, MonadError Error m) => Type -> m (FormatTree, String)
makeInputFormatFromType = \case
  IntTy -> do
    x <- unVarName . value' <$> genVarName'
    return (Exp (Var x), x)
  ListTy t -> do
    n <- unVarName . value' <$> genVarName'
    i <- unVarName . value' <$> genVarName'
    (body, x) <- makeInputFormatFromType t
    body <- (`mapFormatTreeM` body) $ \case
      Exp e -> return $ Exp (At e i)
      format -> return format
    return (Seq [Exp (Var n), Loop i (Var n) body], x)
  t -> throwSemanticError $ "cannot read input of type: " ++ formatType t

makeOutputFormatFromType' :: (MonadAlpha m, MonadError Error m) => Type -> m (FormatTree, String)
makeOutputFormatFromType' = \case
  IntTy -> do
    x <- unVarName . value' <$> genVarName'
    return (Exp (Var x), x)
  ListTy t -> do
    i <- unVarName . value' <$> genVarName'
    (body, x) <- makeOutputFormatFromType' t
    body <- (`mapFormatTreeM` body) $ \case
      Exp e -> return $ Exp (At e i)
      Loop i (Len n) body -> return $ Loop i (Len (At n i)) body
      format -> return format
    return (Seq [Exp (Len (Var x)), Loop i (Len (Var x)) body], x)
  t -> throwSemanticError $ "cannot read input of type: " ++ formatType t

makeOutputFormatFromType :: (MonadAlpha m, MonadError Error m) => Type -> m (FormatTree, Either String [String])
makeOutputFormatFromType = \case
  TupleTy ts -> do
    outputs <- mapM makeOutputFormatFromType' ts
    return (Seq (map fst outputs), Right (map snd outputs))
  t -> second Left <$> makeOutputFormatFromType' t

makeIOFormatFromType :: (MonadAlpha m, MonadError Error m) => [Type] -> Type -> m IOFormat
makeIOFormatFromType ts ret = do
  inputs <- mapM makeInputFormatFromType ts
  (outputTree, outputVariables) <- makeOutputFormatFromType ret
  return $
    IOFormat
      { inputTree = Seq (map fst inputs),
        inputVariables = map snd inputs,
        outputVariables = outputVariables,
        outputTree = outputTree
      }

run :: (MonadAlpha m, MonadError Error m) => Program -> m IOFormat
run prog = wrapError' "Jikka.RestrictedPython.Convert.DefaultMain" $ do
  (_, args, ret, _) <- lookupSolve prog
  makeIOFormatFromType (map snd args) ret
