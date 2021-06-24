{-# LANGUAGE DeriveFunctor #-}

module Jikka.Common.Matrix
  ( Matrix,
    unMatrix,
    makeMatrix,
    makeMatrix',
    matsize,
    matsize',
    matcheck,
    matzero,
    matone,
    matadd,
    matmul,
    matap,
    matscalar,
    matpow,
  )
where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- | `Matrix` is data for matrices.
-- It is guaranteed that internal arrays are not jagged arrays.
newtype Matrix a = Matrix (V.Vector (V.Vector a))
  deriving (Eq, Ord, Show, Functor)

unMatrix :: Matrix a -> V.Vector (V.Vector a)
unMatrix (Matrix a) = a

-- | `matsize` computes the size of a matrix.
matsize :: Matrix a -> (Int, Int)
matsize (Matrix a) = matsize' a

-- | `matsize'` computes the size of a matrix.
-- This assumes inputs are matrices (`matcheck`).
matsize' :: V.Vector (V.Vector a) -> (Int, Int)
matsize' a =
  if V.null a
    then (0, 0)
    else (V.length a, V.length (a V.! 0))

-- | `matcheck` checks a given vector of vectors is a matrix.
-- That is, this returns `False` for jagged arrays.
matcheck :: V.Vector (V.Vector a) -> Bool
matcheck a =
  let (_, w) = matsize' a
   in all (\row -> V.length row == w) (V.toList a)

makeMatrix :: V.Vector (V.Vector a) -> Maybe (Matrix a)
makeMatrix a = if matcheck a then Just (Matrix a) else Nothing

makeMatrix' :: V.Vector (V.Vector a) -> Matrix a
makeMatrix' a = case makeMatrix a of
  Nothing -> error "Jikka.Common.Matrix.makeMatrix': the input is not a matrix"
  Just a -> a

matzero :: Num a => Int -> Matrix a
matzero n = Matrix $ V.replicate n (V.replicate n 0)

matone :: Num a => Int -> Matrix a
matone n = Matrix $ V.generate n (\y -> V.generate n (\x -> if y == x then 1 else 0))

-- | `matadd` calculates the addition \(A + B\) of two matrices \(A, B\).
-- This assumes sizes of inputs match.
matadd :: Num a => Matrix a -> Matrix a -> Matrix a
matadd (Matrix a) (Matrix b) =
  let (h, w) = matsize' a
   in Matrix $ V.generate h (\y -> V.generate w (\x -> (a V.! y V.! x) + (b V.! y V.! x)))

-- | `matmul` calculates the multiplication \(A B\)of two matrices \(A, B\).
-- This assumes sizes of inputs match.
matmul :: Num a => Matrix a -> Matrix a -> Matrix a
matmul (Matrix a) (Matrix b) = runST $ do
  let (h, n) = matsize' a
  let (_, w) = matsize' b
  c <- MV.replicateM h (MV.replicate w 0)
  forM_ [0 .. h - 1] $ \y -> do
    forM_ [0 .. n - 1] $ \z -> do
      forM_ [0 .. w - 1] $ \x -> do
        let delta = (a V.! y V.! z) * (b V.! z V.! x)
        row <- MV.read c y
        MV.modify row (+ delta) x
  Matrix . V.fromList <$> MV.foldrM' (\row c' -> (: c') <$> V.freeze row) [] c

-- | `matap` calculates the multiplication \(A x\) of a matrix \(A\) and a vector \(x\).
-- This assumes sizes of inputs match.
matap :: Num a => Matrix a -> V.Vector a -> V.Vector a
matap (Matrix a) b = runST $ do
  let (h, w) = matsize' a
  c <- MV.replicate h 0
  forM_ [0 .. h - 1] $ \y -> do
    forM_ [0 .. w - 1] $ \x -> do
      let delta = (a V.! y V.! x) * (b V.! x)
      MV.modify c (+ delta) y
  V.freeze c

matscalar :: Num a => a -> Matrix a -> Matrix a
matscalar a (Matrix b) = Matrix $ V.map (V.map (a *)) b

-- | `matpow` calculates the power \(A^k\) of a matrix \(A\) and a natural number \(k\).
-- This assumes inputs are square matrices.
-- This fails for \(k \lt 0\).
matpow :: (Show a, Num a) => Matrix a -> Integer -> Matrix a
matpow _ k | k < 0 = error "cannot calculate a negative power for a monoid"
matpow x k = go unit x k
  where
    unit = let (h, _) = matsize x in matone h
    go y _ 0 = y
    go y x k = go (if k `mod` 2 == 1 then matmul y x else y) (matmul x x) (k `div` 2)
