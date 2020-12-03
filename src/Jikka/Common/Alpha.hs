{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

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

import Control.Monad.State.Strict

class Monad m => MonadAlpha m where
  nextCounter :: m Int

type Alpha = StateT Int (Either String)

instance Monad m => MonadAlpha (StateT Int m) where
  nextCounter = state $ \i -> (i, i + 1)

runAlpha :: Int -> Alpha a -> Either String (a, Int)
runAlpha i a = runStateT a i

evalAlpha :: Int -> Alpha a -> Either String a
evalAlpha i = fmap fst . runAlpha i

evalAlpha' :: Alpha a -> Either String a
evalAlpha' = evalAlpha 0
