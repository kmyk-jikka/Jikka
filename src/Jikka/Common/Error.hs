{-# LANGUAGE FlexibleContexts #-}

module Jikka.Common.Error
  ( module Control.Monad.Except,
    Responsibility (..),
    ErrorGroup (..),
    Error (..),
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
