{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jikka.Common.Parse.OffsideRule
  ( insertIndentTokens,
    IndentSetting (..),
  )
where

import Jikka.Common.Error
import Jikka.Common.Location

data IndentSetting a
  = IndentSetting
      { indentToken :: a,
        dedentToken :: a,
        initialLine :: Int,
        initialColumn :: Int,
        isOpenParenToken :: a -> Bool,
        isCloseParenToken :: a -> Bool,
        allowNoMatchingDedent :: Bool
      }

computeDelta :: forall m a. MonadError Error m => IndentSetting a -> Int -> [Int] -> Loc -> m (Int, [Int], [WithLoc a])
computeDelta setting = go
  where
    go :: Int -> [Int] -> Loc -> m (Int, [Int], [WithLoc a])
    go _ [] _ = throwInternalError "indent state must be non-empty"
    go y (x : xs) loc@(Loc y' x' _) =
      case (compare y y', compare x x') of
        (GT, _) -> throwInternalError "tokens must be in chronological order"
        (EQ, GT) -> throwInternalError "tokens must be in chronological order"
        (EQ, _) -> return (y', x : xs, [])
        (LT, GT) | not (allowNoMatchingDedent setting) && x' `notElem` xs -> throwSyntaxErrorAt loc "no matching <indent> for <dedent>"
        (LT, GT) -> do
          (y', xs', tokens) <- go y xs (Loc y' x' (-1))
          let token = WithLoc (Loc y' (initialColumn setting) (-1)) (dedentToken setting)
          return (y', xs', token : tokens)
        (LT, EQ) -> return (y', x : xs, [])
        (LT, LT) ->
          let token = WithLoc (Loc y' (initialColumn setting) (-1)) (indentToken setting)
           in return (y', x' : x : xs, [token])

insertIndentTokens :: forall m a. MonadError Error m => IndentSetting a -> [WithLoc a] -> m [WithLoc a]
insertIndentTokens setting = go 0 (initialLine setting - 1) [initialColumn setting]
  where
    go :: Int -> Int -> [Int] -> [WithLoc a] -> m [WithLoc a]
    go _ y xs [] =
      let token = WithLoc (Loc (y + 1) 0 (-1)) (dedentToken setting)
       in return $ replicate (length xs - 1) token
    go paren y xs (token : tokens) = case () of
      _ | isOpenParenToken setting (value token) -> (token :) <$> go (paren + 1) y xs tokens
      _ | isCloseParenToken setting (value token) -> (token :) <$> go (paren - 1) y xs tokens
      _ | paren /= 0 -> (token :) <$> go paren y xs tokens
      _ -> do
        (y', xs', delta) <- computeDelta setting y xs (loc token)
        tokens' <- go 0 y' xs' tokens
        return $ delta ++ token : tokens'
