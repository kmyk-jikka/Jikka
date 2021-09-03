{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Language.RewriteRules
-- Description : checks that there are no name conflicts. / 名前衝突がないか検査します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Language.NameCheck
  ( namecheckProgram,
    namecheckToplevelExpr,
    namecheckExpr,
  )
where

import Control.Monad.State.Strict
import Jikka.Common.Error
import Jikka.Core.Format (formatType)
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Util

define :: (MonadState [(VarName, Type)] m, MonadError Error m) => VarName -> Type -> m ()
define x t = do
  env <- get
  case lookup x env of
    Just t' -> throwInternalError $ "name conflict: " ++ unVarName x ++ ": " ++ formatType t ++ " and " ++ unVarName x ++ ": " ++ formatType t'
    Nothing -> put $ (x, t) : env

namecheckExpr' :: (MonadState [(VarName, Type)] m, MonadError Error m) => Expr -> m ()
namecheckExpr' = \case
  Var x -> do
    env <- get
    case lookup x env of
      Nothing -> throwInternalError $ "undefined variable: " ++ unVarName x
      Just _ -> return ()
  Lit _ -> return ()
  App f e -> do
    namecheckExpr' f
    namecheckExpr' e
  Lam x t e -> do
    define x t
    namecheckExpr' e
  Let x t e1 e2 -> do
    namecheckExpr' e1
    define x t
    namecheckExpr' e2
  Assert e1 e2 -> do
    namecheckExpr' e1
    namecheckExpr' e2

namecheckExpr :: MonadError Error m => [(VarName, Type)] -> Expr -> m ()
namecheckExpr env e = wrapError' "Jikka.Core.Language.NameCheck.namecheckExpr" $ do
  evalStateT (namecheckExpr' e) env

namecheckToplevelExpr' :: (MonadState [(VarName, Type)] m, MonadError Error m) => ToplevelExpr -> m ()
namecheckToplevelExpr' = \case
  ResultExpr e -> namecheckExpr' e
  ToplevelLet x t e cont -> do
    namecheckExpr' e
    define x t
    namecheckToplevelExpr' cont
  ToplevelLetRec f args ret body cont -> do
    let t = curryFunTy (map snd args) ret
    define f t
    forM_ args $ \(x, t) -> do
      define x t
    namecheckExpr' body
    namecheckToplevelExpr' cont
  ToplevelAssert e1 e2 -> do
    namecheckExpr' e1
    namecheckToplevelExpr' e2

namecheckToplevelExpr :: MonadError Error m => [(VarName, Type)] -> ToplevelExpr -> m ()
namecheckToplevelExpr env e = wrapError' "Jikka.Core.Language.NameCheck.namecheckToplevelExpr" $ do
  evalStateT (namecheckToplevelExpr' e) env

namecheckProgram :: MonadError Error m => Program -> m ()
namecheckProgram prog = wrapError' "Jikka.Core.Language.NameCheck.namecheckProgram" $ do
  evalStateT (namecheckToplevelExpr' prog) []
