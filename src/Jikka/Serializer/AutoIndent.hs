module Jikka.Serializer.AutoIndent where

indent :: String
indent = "<INDENT>"

dedent :: String
dedent = "<DEDENT>"

makeIndent :: Int -> [String] -> [String]
makeIndent size = go 0
  where
    go :: Int -> [String] -> [String]
    go _ [] = []
    go n (line : lines)
      | line == indent = go (n + size) lines
      | line == dedent = go (n - size) lines
      | otherwise = (replicate n ' ' ++ line) : go n lines
