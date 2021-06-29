module Jikka.Main where

import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Jikka.Common.Error
import Jikka.Common.Format.Error (hPrintError, hPrintErrorWithText)
import qualified Jikka.Main.Subcommand.Convert as Convert
import qualified Jikka.Main.Subcommand.Debug as Debug
import qualified Jikka.Main.Subcommand.Execute as Execute
import Jikka.Main.Target
import Paths_Jikka (version)
import System.Console.GetOpt
import System.Exit (ExitCode (..))
import System.IO (hPutStr, stderr)

data Flag
  = Help
  | Verbose
  | Version
  | Target String
  deriving (Eq, Ord, Show, Read)

data Options = Options
  { verbose :: Bool,
    target :: Maybe Target
  }
  deriving (Eq, Ord, Show, Read)

defaultOptions :: Options
defaultOptions =
  Options
    { verbose = False,
      target = Nothing
    }

header :: String -> String
header progName = "Usage: " ++ progName ++ " [convert | debug | execute] FILE"

options :: [OptDescr Flag]
options =
  [ Option ['h', '?'] ["help"] (NoArg Help) "",
    Option ['v'] ["verbose"] (NoArg Verbose) "",
    Option [] ["version"] (NoArg Version) "",
    Option [] ["target"] (ReqArg Target "TARGET") "\"python\", \"rpython\", \"core\" or \"cxx\""
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
      Left err -> do
        hPrintError stderr err
        return $ ExitFailure 1
      Right opts -> do
        result <- runExceptT $ runSubcommand subcmd opts path
        case result of
          Left err -> do
            text <- liftIO $ T.readFile path
            hPrintErrorWithText stderr text err
            return $ ExitFailure 1
          Right () -> do
            return ExitSuccess
    (_, _, errors) | errors /= [] -> do
      forM_ errors $ \msg -> do
        let err = WithGroup CommandLineError (Error msg)
        hPrintError stderr err
      return $ ExitFailure 1
    _ -> do
      hPutStr stderr usage
      return $ ExitFailure 1

parseFlags :: String -> [Flag] -> Either Error Options
parseFlags _ = go defaultOptions
  where
    go :: Options -> [Flag] -> Either Error Options
    go opts [] = Right opts
    go opts (flag : flags) = case flag of
      Help -> throwCommandLineError "parseFlags is not called when --help is specified"
      Version -> throwCommandLineError "parseFlags is not called when --version is specified"
      Verbose -> go (opts {verbose = True}) flags
      Target target -> do
        target <- parseTarget target
        go (opts {target = Just target}) flags

runSubcommand :: String -> Options -> FilePath -> ExceptT Error IO ()
runSubcommand subcmd opts path = case subcmd of
  "convert" -> do
    input <- liftIO $ T.readFile path
    output <- liftEither $ Convert.run (fromMaybe CPlusPlusTarget (target opts)) path input
    liftIO $ T.putStr output
  "debug" -> Debug.run path
  "execute" -> Execute.run (fromMaybe CoreTarget (target opts)) path
  _ -> throwCommandLineError $ "undefined subcommand: " ++ show subcmd
