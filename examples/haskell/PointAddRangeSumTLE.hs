{-# LANGUAGE BangPatterns #-}

-- https://judge.yosupo.jp/submission/23816

module Main where

import Control.Applicative ((<$))
import Control.Monad (forM_, replicateM, zipWithM_)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString.Char8 as BS
import Data.Functor ((<$>))
import Data.Maybe (catMaybes, fromJust)
import Data.String (fromString)
import qualified Data.Vector.Unboxed.Mutable as V

-- O(1) for query 0; O(n) for query (1)
solve1 :: V.STVector s Int -> (Int, Int, Int) -> ST s (Maybe Int)
solve1 a query = case query of
  (0, p, x) -> do
    V.modify a (+ x) p
    return Nothing
  (1, l, r) -> do
    bs <- mapM (V.read a) [l .. r - 1]
    let !c = sum bs -- remove thunks
    return $ Just c
  _ -> undefined

-- O(n q)
solve :: Int -> Int -> [Int] -> [(Int, Int, Int)] -> ST s [Int]
solve n q a queries = do
  b <- V.unsafeNew n
  zipWithM_ (V.write b) [0 ..] a
  catMaybes <$> mapM (solve1 b) queries

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
