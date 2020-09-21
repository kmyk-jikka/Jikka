{-# LANGUAGE BangPatterns #-}

-- https://judge.yosupo.jp/submission/23821

module Main where

import Control.Applicative ((<$))
import Control.Monad (forM_, replicateM, zipWithM_)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString.Char8 as BS
import Data.Functor ((<$>))
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>), Sum (..))
import Data.String (fromString)
import qualified Data.Vector.Unboxed.Mutable as V

newtype SegmentTree s a = SegmentTree {unSegmentTree :: (Int, V.STVector s a)}

-- O(n)
newSegmentTree :: (Monoid a, V.Unbox a) => [a] -> ST s (SegmentTree s a)
newSegmentTree values = do
  let len = length values
  let k = head $ filter (\k -> len <= 2 ^ k) [0 ..]
  let n = 2 ^ k
  a <- V.replicate (2 * n - 1) mempty
  zipWithM_ (\i value -> V.write a (n + i - 1) value) [0 .. len - 1] values
  forM_ [n -2, n -3 .. 0] $ \i -> do
    left <- V.read a (2 * i + 1)
    right <- V.read a (2 * i + 2)
    V.write a i (left <> right)
  return $ SegmentTree (n, a)

-- O(n log n)
setSegmentTree :: (Monoid a, V.Unbox a) => SegmentTree s a -> Int -> a -> ST s ()
setSegmentTree segtree i value = do
  let (n, a) = unSegmentTree segtree
  let go i = case i of
        0 -> return ()
        _ -> do
          let j = (i - 1) `div` 2
          left <- V.read a (2 * j + 1)
          right <- V.read a (2 * j + 2)
          V.write a j (left <> right)
          go j
  V.write a (i + n - 1) value
  go (i + n - 1)

-- O(n log n)
getSegmentTree :: (Monoid a, V.Unbox a) => SegmentTree s a -> Int -> Int -> ST s a
getSegmentTree segtree l r = do
  let (n, a) = unSegmentTree segtree
  let go i il ir =
        if ir <= l || r <= il
          then return mempty
          else
            if l <= il && ir <= r
              then V.read a i
              else do
                let !im = (il + ir) `div` 2 -- remove thunks
                left <- go (2 * i + 1) il im
                right <- go (2 * i + 2) im ir
                let !value = left <> right -- remove thunks
                return value
  go 0 0 n

-- O(log n)
solve1 :: SegmentTree s (Sum Int) -> (Int, Int, Int) -> ST s (Maybe (Sum Int))
solve1 segtree query = case query of
  (0, p, x) -> do
    value <- getSegmentTree segtree p (p + 1)
    setSegmentTree segtree p (Sum (getSum value + x))
    return Nothing
  (1, l, r) -> do
    Just <$> getSegmentTree segtree l r
  _ -> undefined

-- O(n + q log n)
solve :: Int -> Int -> [Int] -> [(Int, Int, Int)] -> ST s [Int]
solve n q a queries = do
  segtree <- newSegmentTree (map Sum a)
  map getSum . catMaybes <$> mapM (solve1 segtree) queries

main :: IO ()
main = do
  let readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  [n, q] <- readInts
  a <- readInts
  queries <- replicateM q $ do
    [q0, q1, q2] <- readInts
    return (q0, q1, q2)
  let !ans = runST $ solve n q a queries
  forM_ ans $ \x ->
    BS.putStrLn . fromString $ show x
