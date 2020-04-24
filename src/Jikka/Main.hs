module Jikka.Main where

import qualified Data.Text.IO as T
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
header progName = "Usage: " ++ progName ++ " FILE"

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
    (parsed, [path], []) -> do
      main' usage parsed path
      return ExitSuccess
    (_, _, errors) -> do
      mapM_ (\error -> putStr $ name ++ ": " ++ error) errors
      putStrLn ""
      putStr usage
      return $ ExitFailure 1

main' :: String -> [Flag] -> FilePath -> IO ()
main' usage opts path = go defaultOptions opts
  where
    go :: Options -> [Flag] -> IO ()
    go opts [] = main'' opts path
    go opts (flag : flags) = case flag of
      Help -> putStr usage
      Version -> putStrLn $ showVersion version
      Verbose -> go (opts {verbose = True}) flags

main'' :: Options -> FilePath -> IO ()
main'' opts path = do
  code <- T.readFile path
  T.putStr code
