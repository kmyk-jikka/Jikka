module Jikka.Main where

import Control.Monad.Except
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import qualified Jikka.Main.Subcommand.Convert as Convert
import qualified Jikka.Main.Subcommand.Debug as Debug
import qualified Jikka.Main.Subcommand.Execute as Execute
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
header progName = "Usage: " ++ progName ++ " [convert | debug | exec] FILE"

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
        result <- runExceptT $ runSubcommand subcmd opts path
        case result of
          Left error -> do
            hPutStrLn stderr error
            return $ ExitFailure 1
          Right () -> do
            return ExitSuccess
    (_, _, errors) | errors /= [] -> do
      mapM_ (\error -> hPutStr stderr $ name ++ ": " ++ error) errors
      return $ ExitFailure 1
    _ -> do
      hPutStr stderr usage
      return $ ExitFailure 1

parseFlags :: String -> [Flag] -> Either String Options
parseFlags _ = go defaultOptions
  where
    go :: Options -> [Flag] -> Either String Options
    go opts [] = Right opts
    go opts (flag : flags) = case flag of
      Help -> error "parseFlags is not called when --help is specified"
      Version -> error "parseFlags is not called when --version is specified"
      Verbose -> go (opts {verbose = True}) flags

runSubcommand :: String -> Options -> FilePath -> ExceptT String IO ()
runSubcommand subcmd _ path = case subcmd of
  "convert" -> do
    input <- liftIO $ T.readFile path
    output <- liftEither $ Convert.run path input
    liftIO $ T.putStr output
  "debug" -> Debug.run path
  "execute" -> Execute.run path
  _ -> throwError $ "undefined subcommand: " ++ show subcmd
