{-# LANGUAGE ScopedTypeVariables #-}

module Jikka.Deserializer.OffsideRule
  ( insertIndentTokens,
    IndentSetting (..),
  )
where

import Jikka.Deserializer.Pos

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

computeDelta :: forall a. IndentSetting a -> Int -> [Int] -> Pos -> Either String (Int, [Int], [WithPos a])
computeDelta setting = go
  where
    go :: Int -> [Int] -> Pos -> Either String (Int, [Int], [WithPos a])
    go _ [] _ = error "indent state must be non-empty"
    go y (x : xs) pos@(Pos y' x') =
      case (compare y y', compare x x') of
        (GT, _) -> error "tokens must be in chronological order"
        (EQ, GT) -> error "tokens must be in chronological order"
        (EQ, _) -> return (y', x : xs, [])
        (LT, GT) | not (allowNoMatchingDedent setting) && x' `notElem` xs -> Left $ "Lexical error at " ++ prettyPos pos ++ ": no matching <indent> for <dedent>"
        (LT, GT) -> do
          (y', xs', tokens) <- go y xs (Pos y' x')
          let token = WithPos (Pos y' (initialColumn setting)) (dedentToken setting)
          return (y', xs', token : tokens)
        (LT, EQ) -> return (y', x : xs, [])
        (LT, LT) ->
          let token = WithPos (Pos y' (initialColumn setting)) (indentToken setting)
           in return (y', x' : x : xs, [token])

insertIndentTokens :: forall a. IndentSetting a -> [WithPos a] -> Either String [WithPos a]
insertIndentTokens setting = go 0 (initialLine setting - 1) [initialColumn setting]
  where
    go :: Int -> Int -> [Int] -> [WithPos a] -> Either String [WithPos a]
    go _ y xs [] =
      let token = WithPos (Pos (y + 1) 0) (dedentToken setting)
       in return $ replicate (length xs - 1) token
    go paren y xs (token : tokens) = case () of
      _ | isOpenParenToken setting (value token) -> (token :) <$> go (paren + 1) y xs tokens
      _ | isCloseParenToken setting (value token) -> (token :) <$> go (paren - 1) y xs tokens
      _ | paren /= 0 -> (token :) <$> go paren y xs tokens
      _ -> do
        (y', xs', delta) <- computeDelta setting y xs (pos token)
        tokens' <- go 0 y' xs' tokens
        return $ delta ++ token : tokens'
