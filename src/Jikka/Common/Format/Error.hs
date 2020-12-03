{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Common.Format.Error
  ( prettyError,
    prettyErrorWithText,
  )
where

import Data.List (intercalate)
import Data.Text (Text)
import Jikka.Common.Error
import Jikka.Common.Format.Location
import Jikka.Common.Location

prettyError :: Error -> String
prettyError err = intercalate ": " ((group ++ loc ++ resp) : getMessages err)
  where
    group = prettyGroup (getErrorGroup err)
    loc = case getLocation err of
      Nothing -> ""
      Just loc -> " (" ++ prettyLoc loc ++ ")"
    resp = case getResponsibility err of
      Just UserMistake -> " (user's mistake?)"
      Just ImplementationBug -> " (implementation's bug?)"
      Nothing -> ""

prettyErrorWithText :: Text -> Error -> [String]
prettyErrorWithText text err = case getLocation err of
  Nothing -> [prettyError err]
  Just loc -> prettyError err : prettyLocWithText text loc

prettyGroup :: Maybe ErrorGroup -> String
prettyGroup = \case
  Nothing -> "Error"
  Just LexicalError -> "Lexical Error"
  Just SyntaxError -> "Syntax Error"
  Just SemanticError -> "Semantic Error"
  Just SymbolError -> "Symbol Error"
  Just TypeError -> "Type Error"
  Just EvaluationError -> "Evaluation Error"
  Just RuntimeError -> "Runtime Error"
  Just AssertionError -> "Assertion Error"
  Just CommandLineError -> "Command Line Error"
  Just WrongInputError -> "Wrong Input Error"
  Just InternalError -> "Internal Error"

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

getLocation :: Error -> Maybe Loc
getLocation = \case
  Error _ -> Nothing
  WithGroup _ err -> getLocation err
  WithWrapped _ err -> getLocation err
  WithLocation loc _ -> Just loc
  WithResponsibility _ err -> getLocation err

getResponsibilityFromErrorGroup :: ErrorGroup -> Maybe Responsibility
getResponsibilityFromErrorGroup = \case
  CommandLineError -> Nothing
  WrongInputError -> Nothing
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
