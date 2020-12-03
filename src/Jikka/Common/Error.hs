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
    throwSymbolError,
    throwSymbolErrorAt,
    throwTypeError,
    throwSemanticError,
    throwEvaluationError,
    throwRuntimeError,
    throwAssertionError,
    throwCommandLineError,
    throwWrongInputError,
    throwInternalError,
  )
where

import Control.Monad.Except
import Jikka.Common.Language.Pos

data Responsibility
  = UserMistake
  | ImplementationBug
  deriving (Eq, Ord, Show, Read)

data ErrorGroup
  = -- | It's impossible to split the given source text into tokens.
    LexicalError
  | -- | It's impossible to construct AST from tokens.
    SyntaxError
  | -- | There are undefined variables or functions in AST.
    SymbolError
  | -- | It's impossible reconstruct types for AST.
    TypeError
  | -- | other semantic erros
    SemanticError
  | -- | User's program are not ready to evaluate.
    EvaluationError
  | -- | User's program failed while running.
    RuntimeError
  | -- | User's program violates its assertion.
    AssertionError
  | -- | The given command line arguments are not acceptable.
    CommandLineError
  | -- | User's program was correctly running but wrong input text is given.
    WrongInputError
  | -- | It's an bug of implementation.
    InternalError
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

throwSymbolError :: MonadError Error m => String -> m a
throwSymbolError = throwError . WithGroup SymbolError . Error

throwSymbolErrorAt :: MonadError Error m => Pos -> String -> m a
throwSymbolErrorAt loc = throwError . WithLocation loc . WithGroup SymbolError . Error

throwTypeError :: MonadError Error m => String -> m a
throwTypeError = throwError . WithGroup TypeError . Error

throwSemanticError :: MonadError Error m => String -> m a
throwSemanticError = throwError . WithGroup SemanticError . Error

throwEvaluationError :: MonadError Error m => String -> m a
throwEvaluationError = throwError . WithGroup EvaluationError . Error

throwRuntimeError :: MonadError Error m => String -> m a
throwRuntimeError = throwError . WithGroup RuntimeError . Error

throwAssertionError :: MonadError Error m => String -> m a
throwAssertionError = throwError . WithGroup AssertionError . Error

throwCommandLineError :: MonadError Error m => String -> m a
throwCommandLineError = throwError . WithGroup CommandLineError . Error

throwWrongInputError :: MonadError Error m => String -> m a
throwWrongInputError = throwError . WithGroup WrongInputError . Error

throwInternalError :: MonadError Error m => String -> m a
throwInternalError = throwError . WithGroup InternalError . Error
