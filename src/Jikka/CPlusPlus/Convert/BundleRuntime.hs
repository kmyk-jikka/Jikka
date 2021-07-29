{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Jikka.CPlusPlus.Convert.BundleRuntime
-- Description : bundles runtime headers to C++ code. / C++ コードにランタイムヘッダーを埋め込みます。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.CPlusPlus.Convert.BundleRuntime
  ( run,
  )
where

import Control.Monad.State.Strict
import Data.Char
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Jikka.Common.Error
import Paths_Jikka
import System.IO.Error

data PreprocessorState = PreprocessorState
  { definedMacros :: S.Set String,
    ifdefStack :: [Bool]
  }
  deriving (Eq, Ord, Show, Read)

initialPreprocessorState :: PreprocessorState
initialPreprocessorState =
  PreprocessorState
    { definedMacros = S.empty,
      ifdefStack = [True]
    }

throwInternalErrorAt'' :: MonadError Error m => FilePath -> Integer -> String -> m a
throwInternalErrorAt'' path lineno msg = wrapError' (path ++ " (line " ++ show lineno ++ ")") $ throwInternalError msg

runLine :: (MonadIO m, MonadError Error m, MonadState PreprocessorState m) => FilePath -> Integer -> T.Text -> m [T.Text]
runLine path lineno line
  | "#include \"" `T.isPrefixOf` line = case T.splitOn "\"" line of
    ["#include ", path', ""] -> do
      lines <- runFile (T.unpack path')
      return (lines ++ [T.pack ("#line " ++ show (lineno + 1) ++ " \"" ++ path ++ "\"")])
    _ -> throwInternalErrorAt'' path lineno "invalid #include \"...\""
  | otherwise = do
    stk <- gets ifdefStack
    case stk of
      True : _ -> return [line]
      False : _ -> return []
      [] -> throwInternalError "there are more #endif than #ifdef and #ifndef"

runLines :: (MonadIO m, MonadError Error m, MonadState PreprocessorState m) => FilePath -> Integer -> [T.Text] -> m [T.Text]
runLines path lineno lines = concat <$> zipWithM (runLine path) [lineno ..] lines

runFile :: (MonadIO m, MonadError Error m, MonadState PreprocessorState m) => FilePath -> m [T.Text]
runFile path = do
  resolvedPath <- liftIO $ getDataFileName ("runtime/include/" ++ path)
  file <- liftIO $ tryIOError (T.readFile resolvedPath)
  file <- case file of
    Left err -> throwInternalError $ "faild to open file " ++ path ++ ": " ++ show err
    Right file -> return file
  let lines = T.lines file
  let macro = map (\c -> if isAlphaNum c then toUpper c else '_') path
  when (length lines < 3) $ do
    throwInternalErrorAt'' path 1 "file has too few lines"
  when (T.unpack (head lines) /= "#ifndef " ++ macro) $ do
    throwInternalErrorAt'' path 1 $ "the first line must be: #ifndef " ++ macro
  when (T.unpack (lines !! 1) /= "#define " ++ macro) $ do
    throwInternalErrorAt'' path 2 $ "the second line must be: #define " ++ macro
  when (T.unpack (last lines) /= "#endif // " ++ macro) $ do
    throwInternalErrorAt'' path (toInteger (length lines - 1)) $ "the last line must be: #ifndef " ++ macro
  macros <- gets definedMacros
  if macro `S.member` macros
    then return []
    else do
      modify' (\s -> s {definedMacros = S.insert macro macros})
      (T.pack ("#line 3 \"" ++ path ++ "\"") :) <$> runLines path 3 (drop 2 (init lines))

removeConsecutiveLineDirectives :: [T.Text] -> [T.Text]
removeConsecutiveLineDirectives = \case
  (l1 : l2 : lines) | "#line" `T.isPrefixOf` l1 && "#line" `T.isPrefixOf` l2 -> removeConsecutiveLineDirectives (l2 : lines)
  (line : lines) -> line : removeConsecutiveLineDirectives lines
  [] -> []

-- | `run` bundles runtime headers to C++ code like <https://github.com/online-judge-tools/verification-helper `oj-bundle` command>.
run :: (MonadIO m, MonadError Error m) => T.Text -> m T.Text
run prog = wrapError' "Jikka.CPlusPlus.Convert.BundleRuntime" $ do
  lines <- evalStateT (runLines "main.cpp" 1 (T.lines prog)) initialPreprocessorState
  return $ T.unlines (removeConsecutiveLineDirectives lines)
