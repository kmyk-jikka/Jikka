{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jikka.Common.Parse.JoinLines
  ( joinLinesWithParens,
    removeEmptyLines,
  )
where

import Jikka.Common.Error
import Jikka.Common.Location

joinLinesWithParens :: forall m a. (MonadError Error m, Show a) => (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> [WithLoc a] -> m [WithLoc a]
joinLinesWithParens isOpen isClose isNewline = go []
  where
    go :: [WithLoc a] -> [WithLoc a] -> m [WithLoc a]
    go stk tokens = case (stk, tokens) of
      ([], []) -> return []
      (paren : _, []) -> throwLexicalErrorAt (loc paren) $ "unmatching paren found: " ++ show (value paren)
      (_, token : tokens) | isOpen (value token) -> (token :) <$> go (token : stk) tokens
      ([], token : _) | isClose (value token) -> throwLexicalErrorAt (loc token) $ "unmatching paren found: " ++ show (value token)
      (_ : stk, token : tokens) | isClose (value token) -> (token :) <$> go stk tokens
      (_ : _, token : tokens) | isNewline (value token) -> go stk tokens
      (_, token : tokens) -> (token :) <$> go stk tokens

removeEmptyLines :: forall a. (a -> Bool) -> [WithLoc a] -> [WithLoc a]
removeEmptyLines isNewline = go True
  where
    go :: Bool -> [WithLoc a] -> [WithLoc a]
    go _ [] = []
    go lastIsNewline (token : tokens)
      | lastIsNewline && isNewline (value token) = go True tokens
      | otherwise = token : go (isNewline (value token)) tokens
