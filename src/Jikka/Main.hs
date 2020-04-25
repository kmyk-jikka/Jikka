module Jikka.Main where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Jikka.Deserializer.Read as DecAst
import Jikka.Optimizer.Main as Opt
import qualified Jikka.Optimizer.Type.Interface as I
import Jikka.Serializer.Show as SerAst
import Paths_Jikka (version)
import System.Console.GetOpt
import System.Exit (ExitCode (..))
import System.IO (hPutStr, hPutStrLn, stderr)
import qualified Text.Megaparsec as P

data Flag
  = Help
  | Verbose
  | Version
  | From String
  | To String
  deriving (Eq, Ord, Show, Read)

data Options
  = Options
      { verbose :: Bool,
        from :: FilePath -> Text -> Either String I.Expr,
        to :: I.Expr -> Either String Text
      }

defaultOptions :: Options
defaultOptions =
  Options
    { verbose = False,
      from = DecAst.run,
      to = SerAst.run
    }

header :: String -> String
header progName = "Usage: " ++ progName ++ " FILE"

options :: [OptDescr Flag]
options =
  [ Option ['h', '?'] ["help"] (NoArg Help) "",
    Option ['v'] ["verbose"] (NoArg Version) "",
    Option ['f'] ["from"] (ReqArg From "FORMAT") "",
    Option ['t'] ["to"] (ReqArg To "FORMAT") "",
    Option [] ["version"] (NoArg Version) ""
  ]

getDeserializer :: String -> Maybe (FilePath -> Text -> Either String I.Expr)
getDeserializer "ast" = Just DecAst.run
getDeserializer _ = Nothing

getSerializer :: String -> Maybe (I.Expr -> Either String Text)
getSerializer "ast" = Just SerAst.run
getSerializer _ = Nothing

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
    (parsed, [path], []) -> case parseFlags name parsed of
      Left error -> do
        hPutStrLn stderr error
        return $ ExitFailure 1
      Right opts -> do
        input <- T.readFile path
        case main' opts path input of
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
      Help -> undefined
      Version -> undefined
      Verbose -> go (opts {verbose = True}) flags
      From x -> case getDeserializer x of
        Nothing -> Left $ name ++ ": unknown argument for option `--from': " ++ x
        Just f -> go (opts {from = f}) flags
      To y -> case getSerializer y of
        Nothing -> Left $ name ++ ": unknown argument for option `--to': " ++ y
        Just f -> go (opts {to = f}) flags

main' :: Options -> FilePath -> Text -> Either String Text
main' opts path input = do
  expr <- from opts path input
  expr' <- Opt.run expr
  to opts expr'