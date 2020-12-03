module Main where

import qualified Jikka.Main
import System.Environment
import System.Exit

main :: IO ()
main = do
  name <- getProgName
  args <- getArgs
  code <- Jikka.Main.main name args
  exitWith code
