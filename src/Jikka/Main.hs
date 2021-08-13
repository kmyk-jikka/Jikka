{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Jikka.Main
-- Description : is the entry point of the @jikka@ command. / @jikka@ コマンドのエントリポイントです。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Main where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import qualified Jikka.CPlusPlus.Convert.BundleRuntime as BundleRuntime
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
  | Source String
  | Target String
  | BundleRuntimeHeaders Bool
  | EmbedOriginalCode Bool
  deriving (Eq, Ord, Show, Read)

data Options = Options
  { verbose :: Bool,
    source :: Target,
    target :: Maybe Target,
    bundleRuntimeHeaders :: Bool,
    embedOriginalCode :: Bool
  }
  deriving (Eq, Ord, Show, Read)

defaultOptions :: Options
defaultOptions =
  Options
    { verbose = False,
      source = PythonTarget,
      target = Nothing,
      bundleRuntimeHeaders = True,
      embedOriginalCode = True
    }

header :: String -> String
header progName = "Usage: " ++ progName ++ " [convert | debug | execute] FILE"

options :: [OptDescr Flag]
options =
  [ Option ['h', '?'] ["help"] (NoArg Help) "",
    Option ['v'] ["verbose"] (NoArg Verbose) "",
    Option [] ["version"] (NoArg Version) "",
    Option [] ["source"] (ReqArg Source "SOURCE") "\"python\" or \"core\"",
    Option [] ["target"] (ReqArg Target "TARGET") "\"python\", \"rpython\", \"core\" or \"cxx\"",
    Option [] ["bundle-runtime-headers"] (NoArg (BundleRuntimeHeaders True)) "bundles C++ runtime headers",
    Option [] ["no-bundle-runtime-headers"] (NoArg (BundleRuntimeHeaders False)) "",
    Option [] ["embed-original-code"] (NoArg (EmbedOriginalCode True)) "embeds the original Python code",
    Option [] ["no-embed-original-code"] (NoArg (EmbedOriginalCode False)) ""
  ]

main :: String -> [String] -> IO ExitCode
main name args = do
  let usage = usageInfo (header name) options
  case getOpt Permute options args of
    (parsed, _, []) | Help `elem` parsed -> do
      putStr usage
      return ExitSuccess
    (parsed, _, []) | Version `elem` parsed -> do
      putStrLn $ 'v' : showVersion version
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
      Source source -> do
        source <- parseTarget source
        go (opts {source = source}) flags
      Target target -> do
        target <- parseTarget target
        go (opts {target = Just target}) flags
      BundleRuntimeHeaders p -> go (opts {bundleRuntimeHeaders = p}) flags
      EmbedOriginalCode p -> go (opts {embedOriginalCode = p}) flags

runSubcommand :: String -> Options -> FilePath -> ExceptT Error IO ()
runSubcommand subcmd opts path = case subcmd of
  "convert" -> do
    input <- liftIO $ T.readFile path
    let target' = fromMaybe CPlusPlusTarget (target opts)
    output <- liftEither $ Convert.run (source opts) target' path input
    output <-
      if target' == CPlusPlusTarget && bundleRuntimeHeaders opts
        then BundleRuntime.run output
        else return output
    output <-
      return $
        if target' == CPlusPlusTarget && embedOriginalCode opts
          then
            let headers = ["// This C++ code is transpiled using Jikka transpiler v" <> T.pack (showVersion version) <> " https://github.com/kmyk/Jikka", "// The original Python code:"]
             in T.unlines (headers ++ map ("//     " <>) (T.lines input)) <> output
          else output
    liftIO $ T.putStr output
  "debug" -> Debug.run path -- TODO: make this subcommand convenient
  "execute" -> Execute.run (fromMaybe CoreTarget (target opts)) path -- TODO: use source
  _ -> throwCommandLineError $ "undefined subcommand: " ++ show subcmd
