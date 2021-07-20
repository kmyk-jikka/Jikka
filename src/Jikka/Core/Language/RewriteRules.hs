{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Language.RewriteRules
-- Description : provides functions for rewrite rules. / 書き換え規則のための関数を提供します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Language.RewriteRules
  ( RewriteRule (..),
    pureRewriteRule,
    simpleRewriteRule,
    applyRewriteRule,
    applyRewriteRuleToplevelExpr,
    applyRewriteRuleProgram,
    applyRewriteRuleProgram',
    traceRewriteRule,
  )
where

import Data.Maybe (fromMaybe)
import Debug.Trace
import Jikka.Core.Format (formatExpr)
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Util (curryFunTy)

newtype RewriteRule m = RewriteRule ([(VarName, Type)] -> Expr -> m (Maybe Expr))

unRewriteRule :: RewriteRule m -> [(VarName, Type)] -> Expr -> m (Maybe Expr)
unRewriteRule (RewriteRule f) = f

instance Monad m => Semigroup (RewriteRule m) where
  f <> g = RewriteRule $ \env e -> do
    e' <- unRewriteRule f env e
    case e' of
      Nothing -> unRewriteRule g env e
      Just e' -> do
        e'' <- unRewriteRule g env e'
        case e'' of
          Nothing -> return (Just e')
          Just e'' -> return (Just e'')

instance Monad m => Monoid (RewriteRule m) where
  mempty = RewriteRule (\_ _ -> return Nothing)

pureRewriteRule :: Monad m => ([(VarName, Type)] -> Expr -> Maybe Expr) -> RewriteRule m
pureRewriteRule f = RewriteRule (\env e -> return (f env e))

simpleRewriteRule :: Monad m => (Expr -> Maybe Expr) -> RewriteRule m
simpleRewriteRule f = RewriteRule (\_ e -> return (f e))

-- | `applyRewriteRule` applies a given rule to a given expr.
-- This rewrites on all sub-exprs of the given expr, and repeats to rewrite while it is possible.
--
-- * This function is idempotent.
-- * This function doesn't terminate when a given rewrite rule doesn't terminate.
applyRewriteRule :: Monad m => RewriteRule m -> [(VarName, Type)] -> Expr -> m (Maybe Expr)
applyRewriteRule = applyRewriteRulePreOrder

coalesceMaybes :: a -> Maybe a -> b -> Maybe b -> Maybe (a, b)
coalesceMaybes _ Nothing _ Nothing = Nothing
coalesceMaybes a Nothing _ (Just b) = Just (a, b)
coalesceMaybes _ (Just a) b Nothing = Just (a, b)
coalesceMaybes _ (Just a) _ (Just b) = Just (a, b)

applyRewriteRuleToImmediateSubExprs :: Monad m => RewriteRule m -> [(VarName, Type)] -> Expr -> m (Maybe Expr)
applyRewriteRuleToImmediateSubExprs f env = \case
  Var _ -> return Nothing
  Lit _ -> return Nothing
  App e1 e2 -> do
    e1' <- unRewriteRule f env e1
    e2' <- unRewriteRule f env e2
    return $ fmap (uncurry App) (coalesceMaybes e1 e1' e2 e2')
  Lam x t body -> (Lam x t <$>) <$> unRewriteRule f ((x, t) : env) body
  Let x t e1 e2 -> do
    e1' <- unRewriteRule f env e1
    e2' <- unRewriteRule f ((x, t) : env) e2
    return $ fmap (uncurry (Let x t)) (coalesceMaybes e1 e1' e2 e2')

applyRewriteRulePreOrder :: Monad m => RewriteRule m -> [(VarName, Type)] -> Expr -> m (Maybe Expr)
applyRewriteRulePreOrder f env e = do
  e' <- unRewriteRule f env e
  case e' of
    Nothing -> do
      e' <- applyRewriteRuleToImmediateSubExprs (RewriteRule (applyRewriteRulePreOrder f)) env e
      case e' of
        Nothing -> return Nothing
        Just e' -> do
          e'' <- unRewriteRule f env e'
          case e'' of
            Nothing -> return $ Just e'
            Just e'' -> do
              e''' <- applyRewriteRulePreOrder f env e''
              return . Just $ fromMaybe e'' e'''
    Just e' -> do
      e'' <- applyRewriteRulePreOrder f env e'
      return . Just $ fromMaybe e' e''

applyRewriteRuleToplevelExpr :: Monad m => RewriteRule m -> [(VarName, Type)] -> ToplevelExpr -> m (Maybe ToplevelExpr)
applyRewriteRuleToplevelExpr f env = \case
  ResultExpr e -> (ResultExpr <$>) <$> applyRewriteRule f env e
  ToplevelLet y t e cont -> do
    e' <- applyRewriteRule f env e
    cont' <- applyRewriteRuleToplevelExpr f ((y, t) : env) cont
    return $ fmap (uncurry (ToplevelLet y t)) (coalesceMaybes e e' cont cont')
  ToplevelLetRec g args ret body cont -> do
    let env' = (g, curryFunTy (map snd args) ret) : env
    body' <- applyRewriteRule f (reverse args ++ env') body
    cont' <- applyRewriteRuleToplevelExpr f env' cont
    return $ fmap (uncurry (ToplevelLetRec g args ret)) (coalesceMaybes body body' cont cont')

applyRewriteRuleProgram :: Monad m => RewriteRule m -> Program -> m (Maybe Program)
applyRewriteRuleProgram f = applyRewriteRuleToplevelExpr f []

applyRewriteRuleProgram' :: Monad m => RewriteRule m -> Program -> m Program
applyRewriteRuleProgram' f prog = fromMaybe prog <$> applyRewriteRuleProgram f prog

traceRewriteRule :: Monad m => RewriteRule m -> RewriteRule m
traceRewriteRule f = RewriteRule $ \env e -> do
  e' <- unRewriteRule f env e
  case e' of
    Nothing -> return Nothing
    Just e' -> trace ("before:\n" ++ formatExpr e ++ "\nafter:\n" ++ formatExpr e') (return (Just e'))
