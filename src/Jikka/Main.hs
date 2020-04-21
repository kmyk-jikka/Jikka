module Jikka.Main where

import Data.Version (showVersion)
import Paths_Jikka (version)
import System.Console.GetOpt
import System.Exit

data Flag
  = Help
  | Verbose
  | Version
  deriving (Show)

newtype Options
  = Options
      { verbose :: Bool
      }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { verbose = False
    }

header :: String -> String
header progName = "Usage: " ++ progName ++ " < FILE"

options :: [OptDescr Flag]
options =
  [ Option ['h', '?'] ["help"] (NoArg Help) "",
    Option ['v'] ["verbose"] (NoArg Version) "",
    Option [] ["version"] (NoArg Version) ""
  ]

main :: String -> [String] -> IO ExitCode
main name args = do
  let usage = usageInfo (header name) options
  case getOpt Permute options args of
    (parsed, [], []) -> do
      main' usage parsed
      return ExitSuccess
    (_, _, errors) -> do
      mapM_ (\error -> putStr $ name ++ ": " ++ error) errors
      putStrLn ""
      putStr usage
      return $ ExitFailure 1

main' :: String -> [Flag] -> IO ()
main' usage = go defaultOptions
  where
    go :: Options -> [Flag] -> IO ()
    go opts [] = return ()
    go opts (flag : flags) = case flag of
      Help -> putStr usage
      Version -> putStrLn $ showVersion version
      Verbose -> go (opts {verbose = True}) flags
