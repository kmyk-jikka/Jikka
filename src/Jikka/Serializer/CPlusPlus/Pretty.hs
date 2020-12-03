module Jikka.Serializer.CPlusPlus.Pretty (run) where

import Data.List (isPrefixOf, isSuffixOf)
import Jikka.Serializer.CPlusPlus.Type

toLines :: Program -> Either String [String]
toLines prog = error "TODO: implement"

makeIndent :: String -> [String] -> [String]
makeIndent indent = go 0
  where
    go _ [] = []
    go nest (line : lines)
      | "{" `isSuffixOf` line = f nest line : go (nest + 1) lines
      | "}" `isPrefixOf` line = f (nest - 1) line : go (nest - 1) lines
      | otherwise = f nest line : go nest lines
    f nest line = concat (replicate nest indent) ++ line

run :: Program -> Either String String
run prog = do
  lines <- toLines prog
  return . unlines $ makeIndent "    " lines
