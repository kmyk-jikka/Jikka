{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Jikka.Core.Language.RewriteRules
-- Description : provides functions for rewrite rules. / 書き換え規則のための関数を提供します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Language.RewriteRules
  ( RewriteRule,

    -- * Construct Rules
    makeRewriteRule,
    pureRewriteRule,
    simpleRewriteRule,
    traceRewriteRule,

    -- * Apply Rules
    applyRewriteRule,
    applyRewriteRuleToplevelExpr,
    applyRewriteRuleProgram,
    applyRewriteRuleProgram',
  )
where

import Control.Monad.State.Strict
import Data.Maybe
import Debug.Trace
import Jikka.Common.Error
import Jikka.Core.Format (formatExpr)
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Util (curryFunTy)

data RewriteRule m
  = RewriteRule ([(VarName, Type)] -> Expr -> m (Maybe Expr))
  | NamedRule String (RewriteRule m)
  | EmptyRule
  | AltRule (RewriteRule m) (RewriteRule m)
  | TraceRule (RewriteRule m)

instance Monad m => Semigroup (RewriteRule m) where
  f <> g = AltRule f g

instance Monad m => Monoid (RewriteRule m) where
  mempty = EmptyRule

applyRewriteRuleToRootExpr :: MonadError Error m => RewriteRule m -> [(VarName, Type)] -> Expr -> StateT Integer m (Maybe Expr)
applyRewriteRuleToRootExpr f env e = go "(anonymous)" False f
  where
    go :: MonadError Error m => String -> Bool -> RewriteRule m -> StateT Integer m (Maybe Expr)
    go ruleName dumpTrace = \case
      RewriteRule f -> do
        e' <- lift $ f env e
        case e' of
          Nothing -> return ()
          Just e' -> do
            when dumpTrace $ do
              modify' $ trace ("rewrite rule " ++ ruleName ++ "\nbefore:\n" ++ formatExpr e ++ "\nafter:\n" ++ formatExpr e')
            modify' succ
            cnt <- get
            when (cnt >= 100) $ do
              throwInternalError "rewrite rule doesn't terminate"
        return e'
      NamedRule name f -> wrapError' ("rewrite rule " ++ name) $ do
        go name dumpTrace f
      EmptyRule -> return Nothing
      AltRule f g -> do
        e' <- go ruleName dumpTrace f
        case e' of
          Just e' -> return $ Just e'
          Nothing -> go ruleName dumpTrace g
      TraceRule f -> go ruleName True f

makeRewriteRule :: Monad m => String -> ([(VarName, Type)] -> Expr -> m (Maybe Expr)) -> RewriteRule m
makeRewriteRule name f = NamedRule name (RewriteRule f)

pureRewriteRule :: Monad m => String -> ([(VarName, Type)] -> Expr -> Maybe Expr) -> RewriteRule m
pureRewriteRule name f = NamedRule name (RewriteRule (\env e -> return (f env e)))

simpleRewriteRule :: Monad m => String -> (Expr -> Maybe Expr) -> RewriteRule m
simpleRewriteRule name f = NamedRule name (RewriteRule (\_ e -> return (f e)))

-- | `traceRewriteRule` prints logs when the `RewriteRule` works.
traceRewriteRule :: Monad m => RewriteRule m -> RewriteRule m
traceRewriteRule = TraceRule

-- | `applyRewriteRule` applies a given rule to a given expr.
-- This rewrites on all sub-exprs of the given expr, and repeats to rewrite while it is possible.
--
-- * This function is idempotent.
-- * This function doesn't terminate when a given rewrite rule doesn't terminate.
applyRewriteRule :: MonadError Error m => RewriteRule m -> [(VarName, Type)] -> Expr -> m (Maybe Expr)
applyRewriteRule f env e = evalStateT (applyRewriteRule' f env e) 0

applyRewriteRule' :: (MonadError Error m) => RewriteRule m -> [(VarName, Type)] -> Expr -> StateT Integer m (Maybe Expr)
applyRewriteRule' = applyRewriteRulePreOrder

coalesceMaybes :: a -> Maybe a -> b -> Maybe b -> Maybe (a, b)
coalesceMaybes _ Nothing _ Nothing = Nothing
coalesceMaybes a Nothing _ (Just b) = Just (a, b)
coalesceMaybes _ (Just a) b Nothing = Just (a, b)
coalesceMaybes _ (Just a) _ (Just b) = Just (a, b)

applyRewriteRuleToImmediateSubExprs :: MonadError Error m => RewriteRule m -> [(VarName, Type)] -> Expr -> StateT Integer m (Maybe Expr)
applyRewriteRuleToImmediateSubExprs f env = \case
  Var _ -> return Nothing
  Lit _ -> return Nothing
  App e1 e2 -> do
    e1' <- applyRewriteRuleToRootExpr f env e1
    e2' <- applyRewriteRuleToRootExpr f env e2
    return $ fmap (uncurry App) (coalesceMaybes e1 e1' e2 e2')
  Lam x t body -> (Lam x t <$>) <$> applyRewriteRuleToRootExpr f ((x, t) : env) body
  Let x t e1 e2 -> do
    e1' <- applyRewriteRuleToRootExpr f env e1
    e2' <- applyRewriteRuleToRootExpr f ((x, t) : env) e2
    return $ fmap (uncurry (Let x t)) (coalesceMaybes e1 e1' e2 e2')
  Assert e1 e2 -> do
    e1' <- applyRewriteRuleToRootExpr f env e1
    e2' <- applyRewriteRuleToRootExpr f env e2
    return $ fmap (uncurry Assert) (coalesceMaybes e1 e1' e2 e2')

joinStateT :: Monad m => StateT s (StateT s m) a -> StateT s m a
joinStateT f = do
  s <- get
  (a, s) <- runStateT f s
  put s
  return a

applyRewriteRulePreOrder :: forall m. MonadError Error m => RewriteRule m -> [(VarName, Type)] -> Expr -> StateT Integer m (Maybe Expr)
applyRewriteRulePreOrder f env e = do
  e' <- applyRewriteRuleToRootExpr f env e
  case e' of
    Nothing -> do
      let f' = RewriteRule (applyRewriteRulePreOrder f) :: RewriteRule (StateT Integer m)
      e' <- joinStateT (applyRewriteRuleToImmediateSubExprs f' env e)
      case e' of
        Nothing -> return Nothing
        Just e' -> do
          e'' <- applyRewriteRuleToRootExpr f env e'
          case e'' of
            Nothing -> return $ Just e'
            Just e'' -> do
              e''' <- applyRewriteRulePreOrder f env e''
              return . Just $ fromMaybe e'' e'''
    Just e' -> do
      e'' <- applyRewriteRulePreOrder f env e'
      return . Just $ fromMaybe e' e''

applyRewriteRuleToplevelExpr :: MonadError Error m => RewriteRule m -> [(VarName, Type)] -> ToplevelExpr -> StateT Integer m (Maybe ToplevelExpr)
applyRewriteRuleToplevelExpr f env = \case
  ResultExpr e -> (ResultExpr <$>) <$> applyRewriteRule' f env e
  ToplevelLet y t e cont -> do
    e' <- applyRewriteRule' f env e
    cont' <- applyRewriteRuleToplevelExpr f ((y, t) : env) cont
    return $ fmap (uncurry (ToplevelLet y t)) (coalesceMaybes e e' cont cont')
  ToplevelLetRec g args ret body cont -> do
    let env' = (g, curryFunTy (map snd args) ret) : env
    body' <- applyRewriteRule' f (reverse args ++ env') body
    cont' <- applyRewriteRuleToplevelExpr f env' cont
    return $ fmap (uncurry (ToplevelLetRec g args ret)) (coalesceMaybes body body' cont cont')
  ToplevelAssert e1 e2 -> do
    e1' <- applyRewriteRule' f env e1
    e2' <- applyRewriteRuleToplevelExpr f env e2
    return $ fmap (uncurry ToplevelAssert) (coalesceMaybes e1 e1' e2 e2')

applyRewriteRuleProgram :: MonadError Error m => RewriteRule m -> Program -> m (Maybe Program)
applyRewriteRuleProgram f prog = evalStateT (applyRewriteRuleToplevelExpr f [] prog) 0

applyRewriteRuleProgram' :: MonadError Error m => RewriteRule m -> Program -> m Program
applyRewriteRuleProgram' f prog = fromMaybe prog <$> applyRewriteRuleProgram f prog
