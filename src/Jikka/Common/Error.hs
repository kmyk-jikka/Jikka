{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Jikka.Common.Error
-- Description : provides a data type which represents various errors. / 種々のエラーを表現するデータ型を提供します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Common.Error
  ( module Control.Monad.Except,

    -- * error data types
    Responsibility (..),
    ErrorGroup (..),
    Error (..),

    -- * general utilities for `Control.Monad.Except`
    wrapError,
    wrapError',
    wrapAt,
    wrapAt',
    maybeToError,
    eitherToError,
    fromSuccess,

    -- * utilities to report multiple errors
    catchError',
    reportErrors,
    reportErrors2,
    reportErrors3,
    reportErrors4,
    reportErrors5,

    -- * function to construct errors
    lexicalError,
    lexicalErrorAt,
    syntaxError,
    syntaxErrorAt,
    symbolError,
    symbolErrorAt,
    typeError,
    semanticError,
    evaluationError,
    runtimeError,
    assertionError,
    commandLineError,
    wrongInputError,
    internalError,

    -- * actions to throw errors
    throwLexicalError,
    throwLexicalErrorAt,
    throwSyntaxError,
    throwSyntaxErrorAt,
    throwSyntaxErrorAt',
    throwSymbolError,
    throwSymbolErrorAt,
    throwSymbolErrorAt',
    throwTypeError,
    throwTypeErrorAt,
    throwTypeErrorAt',
    throwSemanticError,
    throwSemanticErrorAt,
    throwSemanticErrorAt',
    throwEvaluationError,
    throwRuntimeError,
    throwRuntimeErrorAt,
    throwRuntimeErrorAt',
    throwAssertionError,
    throwCommandLineError,
    throwWrongInputError,
    throwInternalError,
    throwInternalErrorAt,
    throwInternalErrorAt',

    -- * utilities for other types of errors
    bug,
    todo,
  )
where

import Control.Monad.Except
import Data.Either (isRight, lefts, rights)
import Jikka.Common.Location

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
  | ErrorAppend Error Error
  | WithGroup ErrorGroup Error
  | WithWrapped String Error
  | WithLocation Loc Error
  | WithResponsibility Responsibility Error
  deriving (Eq, Ord, Show, Read)

instance Semigroup Error where
  (<>) = ErrorAppend

-- | The list must be non-empty.
errorList :: [Error] -> Error
errorList [] = bug "The list must be non-empty."
errorList (err : errs) = foldl ErrorAppend err errs

wrapError :: MonadError e m => (e -> e) -> m a -> m a
wrapError wrap f = f `catchError` (\err -> throwError (wrap err))

wrapError' :: MonadError Error m => String -> m a -> m a
wrapError' message f = wrapError (WithWrapped message) f

wrapAt :: MonadError Error m => Loc -> m a -> m a
wrapAt loc = wrapError (WithLocation loc)

wrapAt' :: MonadError Error m => Maybe Loc -> m a -> m a
wrapAt' loc = maybe id wrapAt loc

maybeToError :: MonadError a m => a -> Maybe b -> m b
maybeToError a Nothing = throwError a
maybeToError _ (Just b) = return b

eitherToError :: MonadError a m => Either a b -> m b
eitherToError = liftEither

fromSuccess :: Either Error a -> a
fromSuccess f = case f of
  Left err -> error $ "Jikka.Common.Error.fromSuccess: unexpected failure: " ++ show err
  Right y -> y

-- | `catchError'` is the inverse of `liftError`.
catchError' :: MonadError e m => m a -> m (Either e a)
catchError' f = (Right <$> f) `catchError` (\err -> return (Left err))

reportErrors :: MonadError Error m => [Either Error a] -> m [a]
reportErrors xs
  | all isRight xs = return $ rights xs
  | otherwise = throwError $ errorList (lefts xs)

reportErrors2 :: MonadError Error m => Either Error a -> Either Error b -> m (a, b)
reportErrors2 (Right a) (Right b) = return (a, b)
reportErrors2 a b = throwError $ errorList (lefts [() <$ a, () <$ b])

reportErrors3 :: MonadError Error m => Either Error a -> Either Error b -> Either Error c -> m (a, b, c)
reportErrors3 (Right a) (Right b) (Right c) = return (a, b, c)
reportErrors3 a b c = throwError $ errorList (lefts [() <$ a, () <$ b, () <$ c])

reportErrors4 :: MonadError Error m => Either Error a -> Either Error b -> Either Error c -> Either Error d -> m (a, b, c, d)
reportErrors4 (Right a) (Right b) (Right c) (Right d) = return (a, b, c, d)
reportErrors4 a b c d = throwError $ errorList (lefts [() <$ a, () <$ b, () <$ c, () <$ d])

reportErrors5 :: MonadError Error m => Either Error a -> Either Error b -> Either Error c -> Either Error d -> Either Error e -> m (a, b, c, d, e)
reportErrors5 (Right a) (Right b) (Right c) (Right d) (Right e) = return (a, b, c, d, e)
reportErrors5 a b c d e = throwError $ errorList (lefts [() <$ a, () <$ b, () <$ c, () <$ d, () <$ e])

lexicalError :: String -> Error
lexicalError = WithGroup LexicalError . Error

lexicalErrorAt :: Loc -> String -> Error
lexicalErrorAt loc = WithLocation loc . WithGroup LexicalError . Error

syntaxError :: String -> Error
syntaxError = WithGroup SyntaxError . Error

syntaxErrorAt :: Loc -> String -> Error
syntaxErrorAt loc = WithLocation loc . WithGroup SyntaxError . Error

symbolError :: String -> Error
symbolError = WithGroup SymbolError . Error

symbolErrorAt :: Loc -> String -> Error
symbolErrorAt loc = WithLocation loc . WithGroup SymbolError . Error

typeError :: String -> Error
typeError = WithGroup TypeError . Error

semanticError :: String -> Error
semanticError = WithGroup SemanticError . Error

evaluationError :: String -> Error
evaluationError = WithGroup EvaluationError . Error

runtimeError :: String -> Error
runtimeError = WithGroup RuntimeError . Error

assertionError :: String -> Error
assertionError = WithGroup AssertionError . Error

commandLineError :: String -> Error
commandLineError = WithGroup CommandLineError . Error

wrongInputError :: String -> Error
wrongInputError = WithGroup WrongInputError . Error

internalError :: String -> Error
internalError = WithGroup InternalError . Error

throwLexicalError :: MonadError Error m => String -> m a
throwLexicalError = throwError . WithGroup LexicalError . Error

throwLexicalErrorAt :: MonadError Error m => Loc -> String -> m a
throwLexicalErrorAt loc = throwError . WithLocation loc . WithGroup LexicalError . Error

throwSyntaxError :: MonadError Error m => String -> m a
throwSyntaxError = throwError . WithGroup SyntaxError . Error

throwSyntaxErrorAt :: MonadError Error m => Loc -> String -> m a
throwSyntaxErrorAt loc = throwError . WithLocation loc . WithGroup SyntaxError . Error

throwSyntaxErrorAt' :: MonadError Error m => Maybe Loc -> String -> m a
throwSyntaxErrorAt' loc = throwError . maybe id WithLocation loc . WithGroup SyntaxError . Error

throwSymbolError :: MonadError Error m => String -> m a
throwSymbolError = throwError . WithGroup SymbolError . Error

throwSymbolErrorAt :: MonadError Error m => Loc -> String -> m a
throwSymbolErrorAt loc = throwError . WithLocation loc . WithGroup SymbolError . Error

throwSymbolErrorAt' :: MonadError Error m => Maybe Loc -> String -> m a
throwSymbolErrorAt' loc = throwError . maybe id WithLocation loc . WithGroup SymbolError . Error

throwTypeError :: MonadError Error m => String -> m a
throwTypeError = throwError . WithGroup TypeError . Error

throwTypeErrorAt :: MonadError Error m => Loc -> String -> m a
throwTypeErrorAt loc = throwError . WithLocation loc . WithGroup TypeError . Error

throwTypeErrorAt' :: MonadError Error m => Maybe Loc -> String -> m a
throwTypeErrorAt' loc = throwError . maybe id WithLocation loc . WithGroup TypeError . Error

throwSemanticError :: MonadError Error m => String -> m a
throwSemanticError = throwError . WithGroup SemanticError . Error

throwSemanticErrorAt :: MonadError Error m => Loc -> String -> m a
throwSemanticErrorAt loc = throwError . WithLocation loc . WithGroup SemanticError . Error

throwSemanticErrorAt' :: MonadError Error m => Maybe Loc -> String -> m a
throwSemanticErrorAt' loc = throwError . maybe id WithLocation loc . WithGroup SemanticError . Error

throwEvaluationError :: MonadError Error m => String -> m a
throwEvaluationError = throwError . WithGroup EvaluationError . Error

throwRuntimeError :: MonadError Error m => String -> m a
throwRuntimeError = throwError . WithGroup RuntimeError . Error

throwRuntimeErrorAt :: MonadError Error m => Loc -> String -> m a
throwRuntimeErrorAt loc = throwError . WithLocation loc . WithGroup RuntimeError . Error

throwRuntimeErrorAt' :: MonadError Error m => Maybe Loc -> String -> m a
throwRuntimeErrorAt' loc = throwError . maybe id WithLocation loc . WithGroup RuntimeError . Error

throwAssertionError :: MonadError Error m => String -> m a
throwAssertionError = throwError . WithGroup AssertionError . Error

throwCommandLineError :: MonadError Error m => String -> m a
throwCommandLineError = throwError . WithGroup CommandLineError . Error

throwWrongInputError :: MonadError Error m => String -> m a
throwWrongInputError = throwError . WithGroup WrongInputError . Error

throwInternalError :: MonadError Error m => String -> m a
throwInternalError = throwError . WithGroup InternalError . Error

throwInternalErrorAt :: MonadError Error m => Loc -> String -> m a
throwInternalErrorAt loc = throwError . WithLocation loc . WithGroup InternalError . Error

throwInternalErrorAt' :: MonadError Error m => Maybe Loc -> String -> m a
throwInternalErrorAt' loc = throwError . maybe id WithLocation loc . WithGroup InternalError . Error

bug :: String -> a
bug msg = error $ "Fatal Error (implementation's bug): " ++ msg

todo :: String -> a
todo msg = error $ "TODO Error (the feature is not implemented yet): " ++ msg
