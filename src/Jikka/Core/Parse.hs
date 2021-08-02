{-# LANGUAGE FlexibleContexts #-}

module Jikka.Core.Parse
  ( run,
    parseProgram,
    parseExpr,
    parseType,
  )
where

import Data.Text (Text, unpack)
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.Expr
import qualified Jikka.Core.Parse.Alex as L
import qualified Jikka.Core.Parse.Happy as P

parseType :: (MonadAlpha m, MonadError Error m) => String -> m Type
parseType input = do
  tokens <- L.run input
  P.runType tokens

parseExpr :: (MonadAlpha m, MonadError Error m) => String -> m Expr
parseExpr input = do
  tokens <- L.run input
  P.runExpr tokens

parseProgram :: (MonadAlpha m, MonadError Error m) => String -> m Program
parseProgram input = do
  tokens <- L.run input
  P.runProgram tokens

run :: (MonadAlpha m, MonadError Error m) => FilePath -> Text -> m Program
run _ input = do
  tokens <- L.run $ unpack input
  P.runProgram tokens
