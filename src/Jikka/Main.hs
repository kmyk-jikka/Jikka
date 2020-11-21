module Jikka.Main where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import qualified Jikka.Subcommand.Convert as Convert
import qualified Jikka.Subcommand.Execute as Execute
import Paths_Jikka (version)
import System.Console.GetOpt
import System.Exit (ExitCode (..))
import System.IO (hPutStr, hPutStrLn, stderr)

data Flag
  = Help
  | Verbose
  | Version
  deriving (Eq, Ord, Show, Read)

newtype Options
  = Options
      { verbose :: Bool
      }

defaultOptions :: Options
defaultOptions =
  Options
    { verbose = False
    }

header :: String -> String
header progName = "Usage: " ++ progName ++ " [convert | exec] FILE"

options :: [OptDescr Flag]
options =
  [ Option ['h', '?'] ["help"] (NoArg Help) "",
    Option ['v'] ["verbose"] (NoArg Verbose) "",
    Option [] ["version"] (NoArg Version) ""
  ]

main :: String -> [String] -> IO ExitCode
main name args = do
  let usage = usageInfo (header name) options
  case getOpt Permute options args of
    (parsed, _, []) | Help `elem` parsed -> do
      putStr usage
      return ExitSuccess
    (parsed, _, []) | Version `elem` parsed -> do
      putStrLn $ showVersion version
      return ExitSuccess
    (parsed, [subcmd, path], []) -> case parseFlags name parsed of
      Left error -> do
        hPutStrLn stderr error
        return $ ExitFailure 1
      Right opts -> do
        input <- T.readFile path
        result <- runSubcommand subcmd opts path input
        case result of
          Left error -> do
            hPutStrLn stderr error
            return $ ExitFailure 1
          Right output -> do
            T.putStr output
            return ExitSuccess
    (_, _, errors) | errors /= [] -> do
      mapM_ (\error -> hPutStr stderr $ name ++ ": " ++ error) errors
      return $ ExitFailure 1
    _ -> do
      hPutStr stderr usage
      return $ ExitFailure 1

parseFlags :: String -> [Flag] -> Either String Options
parseFlags name = go defaultOptions
  where
    usage = usageInfo (header name) options
    go :: Options -> [Flag] -> Either String Options
    go opts [] = Right opts
    go opts (flag : flags) = case flag of
      Help -> error "parseFlags is not called when --help is specified"
      Version -> error "parseFlags is not called when --version is specified"
      Verbose -> go (opts {verbose = True}) flags

runSubcommand :: String -> Options -> FilePath -> Text -> IO (Either String Text)
runSubcommand subcmd opts path input = case subcmd of
  "convert" -> return $ Convert.run path input
  "execute" -> Execute.run path input
  _ -> return . Left $ "undefined subcommand: " ++ show subcmd
