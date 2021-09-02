{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Jikka.Common.Error

#ifdef JIKKA_EMBED_RUNTIME
import Jikka.Common.FileEmbed (embedDir)
#else
import qualified Data.Text.IO as T
import Paths_Jikka
import System.IO.Error
#endif

-- Pragmas needs type annotations when OverloadedStrings is used. See https://github.com/ndmitchell/hlint/issues/372
{-# ANN module ("HLint: ignore Unused LANGUAGE pragma" :: String) #-}

#ifdef JIKKA_EMBED_RUNTIME
embeddedRuntimeFiles :: [(FilePath, T.Text)]
embeddedRuntimeFiles = $(embedDir "runtime/include")

readRuntimeFile :: MonadError Error m => FilePath -> m T.Text
readRuntimeFile path =
  case lookup ("runtime/include/" ++ path) embeddedRuntimeFiles of
    Just file -> return file
    Nothing -> throwInternalError $ "failed to open file. It may need recompile the binary?: " ++ path

#else
readRuntimeFile :: (MonadIO m, MonadError Error m) => FilePath -> m T.Text
readRuntimeFile path = do
  resolvedPath <- liftIO $ getDataFileName ("runtime/include/" ++ path)
  file <- liftIO $ tryIOError (T.readFile resolvedPath)
  case file of
    Left err -> throwInternalError $ "faild to open file " ++ path ++ ": " ++ show err
    Right file -> return file
#endif

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

runLine :: (MonadError Error m, MonadState PreprocessorState m) => (FilePath -> m T.Text) -> FilePath -> Integer -> T.Text -> m [T.Text]
runLine readRuntimeFile path lineno line
  | "#include \"" `T.isPrefixOf` line = case T.splitOn "\"" line of
    ["#include ", path', ""] -> do
      lines <- runFile readRuntimeFile (T.unpack path')
      return (lines ++ [T.pack ("#line " ++ show (lineno + 1) ++ " \"" ++ path ++ "\"")])
    _ -> throwInternalErrorAt'' path lineno "invalid #include \"...\""
  | otherwise = do
    stk <- gets ifdefStack
    case stk of
      True : _ -> return [line]
      False : _ -> return []
      [] -> throwInternalError "there are more #endif than #ifdef and #ifndef"

runLines :: (MonadError Error m, MonadState PreprocessorState m) => (FilePath -> m T.Text) -> FilePath -> Integer -> [T.Text] -> m [T.Text]
runLines readRuntimeFile path lineno lines = concat <$> zipWithM (runLine readRuntimeFile path) [lineno ..] lines

runFile :: (MonadError Error m, MonadState PreprocessorState m) => (FilePath -> m T.Text) -> FilePath -> m [T.Text]
runFile readRuntimeFile path = do
  file <- readRuntimeFile path
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
      (T.pack ("#line 3 \"" ++ path ++ "\"") :) <$> runLines readRuntimeFile path 3 (drop 2 (init lines))

removeConsecutiveLineDirectives :: [T.Text] -> [T.Text]
removeConsecutiveLineDirectives = \case
  (l1 : l2 : lines) | "#line" `T.isPrefixOf` l1 && "#line" `T.isPrefixOf` l2 -> removeConsecutiveLineDirectives (l2 : lines)
  (line : lines) -> line : removeConsecutiveLineDirectives lines
  [] -> []

-- | `run` bundles runtime headers to C++ code like <https://github.com/online-judge-tools/verification-helper `oj-bundle` command>.
#ifdef JIKKA_EMBED_RUNTIME
run :: MonadError Error m => T.Text -> m T.Text
#else
run :: (MonadIO m, MonadError Error m) => T.Text -> m T.Text
#endif
run prog = wrapError' "Jikka.CPlusPlus.Convert.BundleRuntime" $ do
  lines <- evalStateT (runLines readRuntimeFile "main.cpp" 1 (T.lines prog)) initialPreprocessorState
  return $ T.unlines (removeConsecutiveLineDirectives lines)
