-- https://judge.yosupo.jp/submission/23791
module Main where

import Control.Monad (forM_, replicateM)
import qualified Data.Array as A
import qualified Data.ByteString.Char8 as BS
import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Data.String (fromString)

-- O(n + q)
solve :: Int -> Int -> [Integer] -> [(Int, Int)] -> [Integer]
solve n q a lrs =
  let b = A.listArray (0, n) $ scanl (+) 0 a
   in map (\(l, r) -> b A.! r - b A.! l) lrs

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
