{-# LANGUAGE FlexibleContexts #-}

module Jikka.Core.Language.Runtime where

import Jikka.Common.Error

floorDiv :: MonadError Error m => Integer -> Integer -> m Integer
floorDiv _ 0 = throwRuntimeError "zero div"
floorDiv a b = return (a `div` b)

floorMod :: MonadError Error m => Integer -> Integer -> m Integer
floorMod _ 0 = throwRuntimeError "zero div"
floorMod a b = return (a `mod` b)

ceilDiv :: MonadError Error m => Integer -> Integer -> m Integer
ceilDiv _ 0 = throwRuntimeError "zero div"
ceilDiv a b = return ((a + b - 1) `div` b)

ceilMod :: MonadError Error m => Integer -> Integer -> m Integer
ceilMod _ 0 = throwRuntimeError "zero div"
ceilMod a b = return (a - ((a + b - 1) `div` b) * b)

minimumEither :: (MonadError Error m, Ord a) => [a] -> m a
minimumEither [] = throwRuntimeError "there is no minimum for the empty list"
minimumEither a = return $ minimum a

maximumEither :: (MonadError Error m, Ord a) => [a] -> m a
maximumEither [] = throwRuntimeError "there is no maximum for the empty list"
maximumEither a = return $ maximum a

argminEither :: (MonadError Error m, Ord a) => [a] -> m Integer
argminEither [] = throwRuntimeError "there is no minimum for the empty list"
argminEither a = return $ snd (minimum (zip a [0 ..]))

argmaxEither :: (MonadError Error m, Ord a) => [a] -> m Integer
argmaxEither [] = throwRuntimeError "there is no maximum for the empty list"
argmaxEither a = return $ snd (maximum (zip a [0 ..]))

modinv :: MonadError Error m => Integer -> Integer -> m Integer
modinv a m | m <= 0 || a `mod` m == 0 = throwRuntimeError $ "invalid argument for inv: " ++ show (a, m)
modinv _ _ = throwInternalError "TODO: implement inv()"

modpow :: MonadError Error m => Integer -> Integer -> Integer -> m Integer
modpow _ _ m | m <= 0 = throwRuntimeError $ "invalid argument for modpow: MOD = " ++ show m
modpow a b m = return $ (a ^ b) `mod` m

fact :: MonadError Error m => Integer -> m Integer
fact n | n < 0 = throwRuntimeError $ "invalid argument for fact: " ++ show n
fact n = return $ product [1 .. n]

choose :: MonadError Error m => Integer -> Integer -> m Integer
choose n r | not (0 <= r && r <= n) = throwRuntimeError $ "invalid argument for choose: " ++ show (n, r)
choose n r = return $ product [n - r + 1 .. n] `div` product [1 .. r]

permute :: MonadError Error m => Integer -> Integer -> m Integer
permute n r | not (0 <= r && r <= n) = throwRuntimeError $ "invalid argument for choose: " ++ show (n, r)
permute n r = return $ product [n - r + 1 .. n]

multichoose :: MonadError Error m => Integer -> Integer -> m Integer
multichoose n r | not (0 <= r && r <= n) = throwRuntimeError $ "invalid argument for multichoose: " ++ show (n, r)
multichoose 0 0 = return 1
multichoose n r = choose (n + r - 1) r
