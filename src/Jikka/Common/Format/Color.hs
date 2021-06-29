module Jikka.Common.Format.Color
  ( ColorFlag (..),
    withColor,
    withBold,
    hGetColorFlag,
    Color (..),
  )
where

import System.Console.ANSI
import System.IO (Handle)

data ColorFlag
  = EnableColor
  | DisableColor
  deriving (Eq, Ord, Show, Read)

withColor :: ColorFlag -> Color -> String -> String
withColor DisableColor _ s = s
withColor EnableColor color s = setSGRCode [SetColor Foreground Vivid color] ++ s ++ setSGRCode [SetColor Foreground Dull White]

withBold :: ColorFlag -> String -> String
withBold DisableColor s = s
withBold EnableColor s = setSGRCode [SetConsoleIntensity BoldIntensity] ++ s ++ setSGRCode [SetConsoleIntensity NormalIntensity]

hGetColorFlag :: Handle -> IO ColorFlag
hGetColorFlag handle = do
  supported <- hSupportsANSI handle
  return (if supported then EnableColor else DisableColor)
