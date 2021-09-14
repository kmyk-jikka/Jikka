{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Language.Beta
-- Description : does beta-reduction. / beta 簡約を行います。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Language.Beta
  ( substitute,
    substituteToplevelExpr,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.Util

-- | `substitute` replaces the occrences of the given variable with the given expr. This considers contexts.
--
-- >>> flip evalAlphaT 0 $ substitute (VarName "x") (Lit (LitInt 0)) (Lam (VarName "y") IntTy (Var (VarName "x")))
-- Lam (VarName "y") IntTy (Lit (LitInt 0))
--
-- >>> flip evalAlphaT 0 $ substitute (VarName "x") (Lit (LitInt 0)) (Lam (VarName "x") IntTy (Var (VarName "x")))
-- Lam (VarName "x") IntTy (Var (VarName "x"))
substitute :: MonadAlpha m => VarName -> Expr -> Expr -> m Expr
substitute x e = \case
  Var y -> return $ if y == x then e else Var y
  Lit lit -> return $ Lit lit
  App e1 e2 -> App <$> substitute x e e1 <*> substitute x e e2
  Lam y t body ->
    if x == y
      then return $ Lam y t body
      else do
        (y, body) <- resolveConflict e (y, body)
        Lam y t <$> substitute x e body
  Let y t e1 e2 -> do
    e1 <- substitute x e e1
    if y == x
      then return $ Let y t e1 e2
      else do
        (y, e2) <- resolveConflict e (y, e2)
        Let y t e1 <$> substitute x e e2
  Assert e1 e2 -> Assert <$> substitute x e e1 <*> substitute x e e2

substituteToplevelExpr :: (MonadAlpha m, MonadError Error m) => VarName -> Expr -> ToplevelExpr -> m ToplevelExpr
substituteToplevelExpr x e = \case
  ResultExpr e' -> ResultExpr <$> substitute x e e'
  ToplevelLet y t e' cont -> do
    e' <- substitute x e e'
    if y == x
      then return $ ToplevelLet y t e' cont
      else do
        when (y `isFreeVar` e) $ do
          throwInternalError $ "Jikka.Core.Language.Beta.substituteToplevelExpr: toplevel name conflicts: " ++ formatVarName y
        ToplevelLet y t e' <$> substituteToplevelExpr x e cont
  ToplevelLetRec f args ret body cont -> do
    if f == x
      then return $ ToplevelLetRec f args ret body cont
      else do
        when (f `isFreeVar` e) $ do
          throwInternalError $ "Jikka.Core.Language.Beta.substituteToplevelExpr: toplevel name conflicts: " ++ formatVarName f
        (args, body) <-
          if x `elem` map fst args
            then return (args, body)
            else do
              let go (args, body) (y, t) = do
                    (y, body) <- resolveConflict e (y, body)
                    return (args ++ [(y, t)], body)
              foldM go ([], body) args
        ToplevelLetRec f args ret body <$> substituteToplevelExpr x e cont
  ToplevelAssert e1 e2 -> ToplevelAssert <$> substitute x e e1 <*> substituteToplevelExpr x e e2

resolveConflict :: MonadAlpha m => Expr -> (VarName, Expr) -> m (VarName, Expr)
resolveConflict e (x, e') =
  if x `isFreeVar` e
    then do
      y <- genVarName x
      e' <- substitute x (Var y) e'
      return (y, e')
    else return (x, e')
