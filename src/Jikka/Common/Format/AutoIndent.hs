module Jikka.Common.Format.AutoIndent where

import Data.List (isPrefixOf, isSuffixOf)

indent :: String
indent = "<INDENT>"

dedent :: String
dedent = "<DEDENT>"

makeIndentFromMarkers :: Int -> [String] -> [String]
makeIndentFromMarkers size = go 0
  where
    go :: Int -> [String] -> [String]
    go _ [] = []
    go n (line : lines)
      | line == indent = go (n + size) lines
      | line == dedent = go (n - size) lines
      | otherwise = (replicate n ' ' ++ line) : go n lines

makeIndentFromBraces :: Int -> [String] -> [String]
makeIndentFromBraces size = makeIndentFromMarkers size . insertIndentDedentFromBraces

insertIndentDedentFromBraces :: [String] -> [String]
insertIndentDedentFromBraces = concatMap go
  where
    go :: String -> [String]
    go line =
      let close = if "}" `isPrefixOf` line then [dedent] else []
          open = if "{" `isSuffixOf` line then [indent] else []
       in close ++ [line] ++ open
