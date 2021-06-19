{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Jikka.Common.Alpha
-- Description : provides a monad to run alpha-conversion.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Common.Alpha` provides a monad to run alpha-conversion. This monad has only a feature to make unique numbers.
module Jikka.Common.Alpha where

import Control.Arrow (first)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Signatures
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

class Monad m => MonadAlpha m where
  nextCounter :: m Int

newtype AlphaT m a = AlphaT {runAlphaT :: Int -> m (a, Int)}

instance Monad m => MonadAlpha (AlphaT m) where
  nextCounter = AlphaT (\i -> return (i, i + 1))

instance Functor m => Functor (AlphaT m) where
  fmap f (AlphaT x) = AlphaT (\i -> fmap (first f) (x i))

instance Monad m => Applicative (AlphaT m) where
  pure x = AlphaT (\i -> pure (x, i))
  AlphaT f <*> AlphaT x = AlphaT $ \i -> do
    (f, i) <- f i
    (x, i) <- x i
    return (f x, i)

instance Monad m => Monad (AlphaT m) where
  AlphaT x >>= f = AlphaT $ \i -> do
    (x, i) <- x i
    runAlphaT (f x) i

instance MonadFix m => MonadFix (AlphaT m) where
  mfix f = AlphaT (\i -> mfix (\(x, _) -> runAlphaT (f x) i))

liftCatch :: Catch e m (a, Int) -> Catch e (AlphaT m) a
liftCatch catchE m h = AlphaT (\i -> runAlphaT m i `catchE` \e -> runAlphaT (h e) i)

instance MonadTrans AlphaT where
  lift m = AlphaT $ \i -> do
    a <- m
    return (a, i)

instance MonadError e m => MonadError e (AlphaT m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

instance MonadIO m => MonadIO (AlphaT m) where
  liftIO = lift . liftIO

evalAlphaT :: Functor m => AlphaT m a -> Int -> m a
evalAlphaT f i = fst <$> runAlphaT f i

instance MonadAlpha m => MonadAlpha (ExceptT e m) where
  nextCounter = lift nextCounter

instance MonadAlpha m => MonadAlpha (ReaderT r m) where
  nextCounter = lift nextCounter

instance MonadAlpha m => MonadAlpha (StateT s m) where
  nextCounter = lift nextCounter

instance (MonadAlpha m, Monoid w) => MonadAlpha (WriterT w m) where
  nextCounter = lift nextCounter
