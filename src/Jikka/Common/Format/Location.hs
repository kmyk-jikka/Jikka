module Jikka.Common.Format.Location
  ( prettyLoc,
    prettyLocWithText,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Jikka.Common.Format.Color
import Jikka.Common.Location

prettyLoc :: Loc -> String
prettyLoc loc = "line " ++ show (line loc) ++ " column " ++ show (column loc)

prettyLocWithText :: ColorFlag -> Text -> Loc -> [String]
prettyLocWithText color text (Loc y x width) = result
  where
    lines :: [Text]
    lines = T.lines text
    paddingSize :: Int
    paddingSize = length $ show (y + 1)
    padRight :: String -> String
    padRight s = s ++ replicate (paddingSize - length s) ' '
    prettyLine :: (String -> String) -> Int -> [String]
    prettyLine f y
      | 1 <= y && y <= length lines = [withColor color Blue (padRight (show y) ++ " |") ++ f (T.unpack (lines !! (y - 1)))]
      | otherwise = []
    result :: [String]
    result
      | 1 <= y && y <= length lines =
        concat
          [ prettyLine id (y - 1),
            prettyLine (\line -> take (x - 1) line ++ withColor color Red (take width (drop (x - 1) line)) ++ drop (x - 1 + width) line) y,
            [replicate (paddingSize + 2 + x - 1) ' ' ++ withColor color Red (replicate (max 1 width) '^')],
            prettyLine id (y + 1)
          ]
      | otherwise = ["<invalid loc>"]
