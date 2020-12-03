module Jikka.Common.Format.Location (prettyLoc, prettyLocWithText) where

import Data.Text (Text)
import qualified Data.Text as T
import Jikka.Common.Location

prettyLoc :: Loc -> String
prettyLoc loc = "line " ++ show (line loc) ++ " column " ++ show (column loc)

prettyLocWithText :: Text -> Loc -> [String]
prettyLocWithText text (Loc y x width) = result
  where
    lines :: [Text]
    lines = T.lines text
    paddingSize :: Int
    paddingSize = length $ show (y + 1)
    padRight :: String -> String
    padRight s = s ++ replicate (paddingSize - length s) ' '
    prettyLine :: Int -> [String]
    prettyLine y
      | 1 <= y && y <= length lines = [padRight (show y) ++ " |" ++ T.unpack (lines !! (y - 1))]
      | otherwise = []
    result :: [String]
    result
      | 1 <= y && y <= length lines =
        concat
          [ prettyLine (y - 1),
            prettyLine y,
            [replicate (paddingSize + 2 + x - 1) ' ' ++ replicate (max 1 width) '^'],
            prettyLine (y + 1)
          ]
      | otherwise = ["<invalid loc>"]
