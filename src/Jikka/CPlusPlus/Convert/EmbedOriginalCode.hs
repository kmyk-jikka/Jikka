{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Convert.EmbedOriginalCode where

import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_Jikka (version)

run' :: T.Text -> T.Text
run' input =
  let headers =
        [ "// This C++ code is transpiled using Jikka transpiler v" <> T.pack (showVersion version) <> " https://github.com/kmyk/Jikka",
          "// The original Python code:"
        ]
   in T.unlines (headers ++ map ("//     " <>) (T.lines input))

run :: T.Text -> T.Text -> T.Text
run input output = run' input <> output
