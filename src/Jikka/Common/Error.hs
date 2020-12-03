{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Common.Error
  ( module Control.Monad.Except,
    Responsibility (..),
    ErrorGroup (..),
    Error (..),
    prettyError,
    wrapError,
    wrapError',
    maybeToError,
    eitherToError,
    throwLexicalError,
    throwLexicalErrorAt,
    throwSyntaxError,
    throwSyntaxErrorAt,
    throwSemanticError,
    throwInternalError,
    throwEvaluationError,
    throwRuntimeError,
    throwCommandLineError,
  )
where

import Control.Monad.Except
import Data.List (intercalate)
import Jikka.Common.Language.Pos

data Responsibility
  = UserMistake
  | ImplementationBug
  deriving (Eq, Ord, Show, Read)

data ErrorGroup
  = LexicalError
  | SyntaxError
  | SemanticError
  | InternalError
  | EvaluationError
  | RuntimeError
  | CommandLineError
  deriving (Eq, Ord, Show, Read)

data Error
  = Error String
  | WithGroup ErrorGroup Error
  | WithWrapped String Error
  | WithLocation Pos Error
  | WithResponsibility Responsibility Error
  deriving (Eq, Ord, Show, Read)

-- -----------------------------------------------------------------------------
-- pretty printing

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

-- -----------------------------------------------------------------------------
-- utility

wrapError :: MonadError e m => (e -> e) -> m a -> m a
wrapError wrap f = f `catchError` (\err -> throwError (wrap err))

wrapError' :: MonadError Error m => String -> m a -> m a
wrapError' message f = wrapError (WithWrapped message) f

maybeToError :: MonadError a m => a -> Maybe b -> m b
maybeToError a Nothing = throwError a
maybeToError _ (Just b) = return b

eitherToError :: MonadError a m => Either a b -> m b
eitherToError (Left a) = throwError a
eitherToError (Right b) = return b

throwLexicalError :: MonadError Error m => String -> m a
throwLexicalError = throwError . WithGroup LexicalError . Error

throwLexicalErrorAt :: MonadError Error m => Pos -> String -> m a
throwLexicalErrorAt loc = throwError . WithLocation loc . WithGroup LexicalError . Error

throwSyntaxError :: MonadError Error m => String -> m a
throwSyntaxError = throwError . WithGroup SyntaxError . Error

throwSyntaxErrorAt :: MonadError Error m => Pos -> String -> m a
throwSyntaxErrorAt loc = throwError . WithLocation loc . WithGroup SyntaxError . Error

throwSemanticError :: MonadError Error m => String -> m a
throwSemanticError = throwError . WithGroup SemanticError . Error

throwInternalError :: MonadError Error m => String -> m a
throwInternalError = throwError . WithGroup InternalError . Error

throwEvaluationError :: MonadError Error m => String -> m a
throwEvaluationError = throwError . WithGroup EvaluationError . Error

throwRuntimeError :: MonadError Error m => String -> m a
throwRuntimeError = throwError . WithGroup RuntimeError . Error

throwCommandLineError :: MonadError Error m => String -> m a
throwCommandLineError = throwError . WithGroup CommandLineError . Error
