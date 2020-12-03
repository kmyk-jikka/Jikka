{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Common.Format.Error
  ( prettyError,
  )
where

import Data.List (intercalate)
import Jikka.Common.Error
import Jikka.Common.Language.Pos

prettyError :: Error -> String
prettyError err = intercalate ": " ((group ++ loc ++ resp) : getMessages err)
  where
    group = case getErrorGroup err of
      Nothing -> "Error"
      Just LexicalError -> "Lexical Error"
      Just SyntaxError -> "Syntax Error"
      Just SemanticError -> "Semantic Error"
      Just InternalError -> "Internal Error"
      Just EvaluationError -> "Evaluation Error"
      Just RuntimeError -> "Runtime Error"
      Just CommandLineError -> "Command Line Error"
    loc = case getLocation err of
      Nothing -> ""
      Just loc -> " (" ++ prettyPos loc ++ ")"
    resp = case getResponsibility err of
      Just UserMistake -> " (user's mistake?)"
      Just ImplementationBug -> " (implementation's bug?)"
      Nothing -> ""

getMessages :: Error -> [String]
getMessages = \case
  Error message -> [message]
  WithGroup _ err -> getMessages err
  WithWrapped message err -> message : getMessages err
  WithLocation _ err -> getMessages err
  WithResponsibility _ err -> getMessages err

getErrorGroup :: Error -> Maybe ErrorGroup
getErrorGroup = \case
  Error _ -> Nothing
  WithGroup group _ -> Just group
  WithWrapped _ err -> getErrorGroup err
  WithLocation _ err -> getErrorGroup err
  WithResponsibility _ err -> getErrorGroup err

getLocation :: Error -> Maybe Pos
getLocation = \case
  Error _ -> Nothing
  WithGroup _ err -> getLocation err
  WithWrapped _ err -> getLocation err
  WithLocation loc _ -> Just loc
  WithResponsibility _ err -> getLocation err

getResponsibilityFromErrorGroup :: ErrorGroup -> Maybe Responsibility
getResponsibilityFromErrorGroup = \case
  InternalError -> Just ImplementationBug
  _ -> Just UserMistake

getResponsibility :: Error -> Maybe Responsibility
getResponsibility = \case
  Error _ -> Nothing
  WithGroup group err -> case getResponsibility err of
    Just resp -> Just resp
    Nothing -> getResponsibilityFromErrorGroup group
  WithWrapped _ err -> getResponsibility err
  WithLocation _ err -> getResponsibility err
  WithResponsibility resp _ -> Just resp
