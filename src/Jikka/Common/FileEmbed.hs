{-# LANGUAGE TemplateHaskell #-}

module Jikka.Common.FileEmbed where

import Control.Monad
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive path = do
  paths <- map ((path ++ "/") ++) <$> listDirectory path
  paths <- forM paths $ \path -> do
    isDir <- doesDirectoryExist path
    if isDir
      then listDirectoryRecursive path
      else return [path]
  return $ concat paths

-- | `embedDir` find files recursively and embed their contents, like https://hackage.haskell.org/package/file-embed @file-embed@>'s <https://hackage.haskell.org/package/file-embed/docs/Data-FileEmbed.html#v:embedDir @embedDir@>.
--
-- == Usage
--
-- > myDir :: [(FilePath, Data.Text.Text)]
-- > myDir = $(embedDir "dirName")
embedDir :: FilePath -> Q Exp
embedDir path = do
  paths <- runIO $ listDirectoryRecursive path
  contents <- runIO $ mapM T.readFile paths :: Q [T.Text]
  mapM_ addDependentFile paths
  let contents' = map (map ord . T.unpack) contents -- use [Int] instead of T.Text for scripts/erase_template_haskell.py
  [e|zip paths (map (T.pack . map chr) contents') :: [(FilePath, T.Text)]|]
