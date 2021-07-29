{-# LANGUAGE FlexibleContexts #-}

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

import Control.Exception
import Control.Monad.State.Strict
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Jikka.Common.Error
import Paths_Jikka
import System.Directory
import System.IO
import System.IO.Error
import System.Process

-- | `run` bundles runtime headers to C++ code using <https://github.com/online-judge-tools/verification-helper `oj-bundle` command>.
run :: (MonadIO m, MonadError Error m) => T.Text -> m T.Text
run prog = wrapError' "Jikka.CPlusPlus.Convert.BundleRuntime" $ do
  command <- liftIO $ findExecutable "oj-bundle"
  command <- case command of
    Nothing -> throwCommandLineError "oj-bundle command is not found. Please install it with $ pip3 install online-judge-verify-helper"
    Just command -> return command
  result <- liftIO . tryIOError $ do
    tempDir <- getTemporaryDirectory
    includePath <- getDataFileName "runtime/include"
    (tempfile, fh) <- openTempFile tempDir "main.cpp"
    (`finally` removeFile tempfile) $ do
      T.hPutStr fh prog
      hClose fh
      readProcess command ["-I", includePath, tempfile] ""
  case result of
    Left err -> throwInternalError (show err)
    Right prog -> return $ T.pack prog
