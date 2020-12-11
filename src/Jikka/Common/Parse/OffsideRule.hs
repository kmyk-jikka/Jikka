{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jikka.Common.Parse.OffsideRule
  ( insertIndents,
  )
where

import Jikka.Common.Error
import Jikka.Common.Location

splitToLines :: forall a. (a -> Bool) -> [WithLoc a] -> [[WithLoc a]]
splitToLines isNewline = go []
  where
    go :: [WithLoc a] -> [WithLoc a] -> [[WithLoc a]]
    go [] [] = []
    go acc [] = [reverse acc]
    go acc (token : tokens)
      | isNewline (value token) = reverse (token : acc) : go [] tokens
      | otherwise = go (token : acc) tokens

insertIndents' :: forall m a. (MonadError Error m, Show a) => a -> a -> [[WithLoc a]] -> m [WithLoc a]
insertIndents' indent dedent = go [1]
  where
    go :: [Int] -> [[WithLoc a]] -> m [WithLoc a]
    go stk tokens = case (stk, tokens) of
      ([1], []) -> return []
      (_ : stk, []) -> (WithLoc (Loc 0 1 0) dedent :) <$> go stk []
      (_, [] : _) -> throwInternalError "a line must be non-empty"
      (_, (token : _) : _) | column (loc token) < 0 -> throwInternalError $ "column must be 1-based for insertIndents': " ++ show token
      ([], _) -> throwInternalError "too many dedents"
      (x : stk, line@(token : _) : tokens') -> case compare x (column (loc token)) of
        LT -> (withLoc (loc token) indent :) . (line ++) <$> go (column (loc token) : x : stk) tokens'
        EQ -> (line ++) <$> go (x : stk) tokens'
        GT -> case stk of
          [] -> throwInternalError "too many dedents"
          (x' : _)
            | x' < column (loc token) -> throwLexicalErrorAt (loc token) $ "unindent does not match any outer indentation level: " ++ show token
            | otherwise -> (withLoc (loc token) dedent :) <$> go stk (line : tokens')
    withLoc :: Loc -> a -> WithLoc a
    withLoc (Loc y x _) a = WithLoc (Loc y x 0) a

-- | `insertIndents` inserts @INDENT@ and @DEDENT@ tokens with Python's way (<https://docs.python.org/3/reference/lexical_analysis.html#indentation>). The `column` of `Loc` must be 1-based. This doen't use physical `line` of `Loc` because logical lines are used for indentation.
insertIndents :: forall m a. (MonadError Error m, Show a) => a -> a -> (a -> Bool) -> [WithLoc a] -> m [WithLoc a]
insertIndents indent dedent isNewline tokens = wrapError' "Jikka.Common.Parse.OffsideRule failed" $ do
  let lines = splitToLines isNewline tokens
  insertIndents' indent dedent lines
