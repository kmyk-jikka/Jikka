{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Jikka.Core.Language.Eta
-- Description : does eta-reductions and eta-expansions. / eta 簡約および eta 展開を行います。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Language.Eta
  ( etaExpand,
    etaExpand',
    etaReduce,
    etaReduce',
  )
where

import Control.Monad.Trans.Maybe
import Data.Maybe
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars
import Jikka.Core.Language.TypeCheck
import Jikka.Core.Language.Util

etaExpand' :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> m (Maybe Expr)
etaExpand' env e = runMaybeT $ do
  t <- lift $ typecheckExpr env e
  ts <- case uncurryFunTy t of
    (ts@(_ : _), _) -> return ts
    _ -> hoistMaybe Nothing
  let (args, body) = uncurryLam e
  guard $ length args /= length ts -- otherwise, it's already eta-expanded
  args' <- forM (drop (length args) ts) $ \t -> do
    x <- lift genVarName'
    return (x, t)
  return $ curryLam (args ++ args') (uncurryApp body (map (Var . fst) args'))

-- `etaExpand` does an eta-expansion.
--
-- == Examples
--
-- Before:
--
-- > map
--
-- After:
--
-- > fun f xs -> map f xs
--
-- Before:
--
-- > let f x y = x + y in f
--
-- After:
--
-- > fun x y -> (let f x y = x + y in f) x y
etaExpand :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> m Expr
etaExpand env e = fromMaybe e <$> etaExpand' env e

etaReduce' :: Expr -> Maybe Expr
etaReduce' e = do
  let (args, body) = uncurryLam e
  let (f, args') = curryApp body
  guard $ length args <= length args'
  let k = length args' - length args
  let f' = uncurryApp f (take k args')
  let args'' = drop k args'
  guard $ args'' == map (Var . fst) args
  guard $ all (\(x, _) -> x `isUnusedVar` f') args
  return f'

-- `etaReduce` does an eta-reduce in the result expr.
--
-- == Examples
--
-- Before:
--
-- > fun f xs -> map f xs
--
-- After:
--
-- > map
--
-- Before:
--
-- > fun x y -> (let f x y = x + y in f) x y
--
-- After:
--
-- > let f x y = x + y in f
etaReduce :: Expr -> Expr
etaReduce e = fromMaybe e (etaReduce' e)
