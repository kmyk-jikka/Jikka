-- https://judge.yosupo.jp/submission/23794
module Main where

import Control.Monad (forM_, replicateM)
import qualified Data.ByteString.Char8 as BS
import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Data.String (fromString)

-- O(n^2). The list access (!!) takes O(n) and this calls it O(n) times.
solve1 :: Int -> [Integer] -> Int -> Int -> Integer
solve1 n a l r = sum $ map (\i -> a !! i) [l .. r - 1]

-- O(n^2 q) in total
solve :: Int -> Int -> [Integer] -> [(Int, Int)] -> [Integer]
solve n q a lrs = map (\(l, r) -> solve1 n a l r) lrs

main :: IO ()
main = do
  let readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  [n, q] <- readInts
  a <- map toInteger <$> readInts
  lrs <- replicateM q $ do
    [l, r] <- readInts
    return (l, r)
  let ans = solve n q a lrs
  forM_ ans $ \x ->
    BS.putStrLn . fromString $ show x
