module Jikka.Common.IO where

import Data.Char (isSpace)
import System.IO

hTakeWhile :: Handle -> (Char -> Bool) -> IO String
hTakeWhile handle pred = do
  isEOF <- hIsEOF handle
  if isEOF
    then return ""
    else do
      c <- hLookAhead handle
      if pred c
        then do
          _ <- hGetChar handle
          (c :) <$> hTakeWhile handle pred
        else return ""

hGetWord :: Handle -> IO String
hGetWord handle = do
  hTakeWhile handle isSpace
  c <- hGetChar handle
  s <- hTakeWhile handle (not . isSpace)
  return (c : s)

getWord :: IO String
getWord = hGetWord stdin
