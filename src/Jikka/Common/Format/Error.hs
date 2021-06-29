{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Common.Format.Error
  ( prettyError,
    prettyErrorWithText,
    hPrintError,
    hPrintErrorWithText,
  )
where

import Data.List (intercalate)
import Data.Text (Text)
import Jikka.Common.Error
import Jikka.Common.Format.Color
import Jikka.Common.Format.Location
import Jikka.Common.Location
import System.IO (Handle, hPutStrLn)

-- | `unpackCombinedErrors` removes `ErrorAppend` ctor from the given `Error`.
unpackCombinedErrors :: Error -> [Error]
unpackCombinedErrors = go
  where
    go :: Error -> [Error]
    go = \case
      err@(Error _) -> [err]
      ErrorAppend err1 err2 -> go err1 ++ go err2
      WithGroup group err -> map (WithGroup group) (go err)
      WithWrapped msg err -> map (WithWrapped msg) (go err)
      WithLocation loc err -> map (WithLocation loc) (go err)
      WithResponsibility resp err -> map (WithResponsibility resp) (go err)

prettyError :: ColorFlag -> Error -> [String]
prettyError color = map (prettyError1 color) . unpackCombinedErrors

-- | @err@ must not have `ErrorAppend`.
prettyError1 :: ColorFlag -> Error -> String
prettyError1 color err = intercalate ": " ((group ++ loc ++ resp) : getMessages err)
  where
    group = withColor color Red $ prettyGroup (getErrorGroup err)
    loc = case getLocation err of
      Nothing -> ""
      Just loc -> " (" ++ prettyLoc loc ++ ")"
    resp = case getResponsibility err of
      Just UserMistake -> " (user's mistake?)"
      Just ImplementationBug -> " (implementation's bug?)"
      Nothing -> ""

prettyErrorWithText :: ColorFlag -> Text -> Error -> [String]
prettyErrorWithText color text = intercalate [""] . map (prettyErrorWithText1 color text) . unpackCombinedErrors

-- | @err@ must not have `ErrorAppend`.
prettyErrorWithText1 :: ColorFlag -> Text -> Error -> [String]
prettyErrorWithText1 color text err = case getLocation err of
  Nothing -> [prettyError1 color err]
  Just loc -> prettyError1 color err : prettyLocWithText color text loc

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

-- | @err@ must not have `ErrorAppend`.
getMessages :: Error -> [String]
getMessages = \case
  Error message -> [message]
  ErrorAppend _ _ -> bug "ErrorAppend is not allowed here."
  WithGroup _ err -> getMessages err
  WithWrapped message err -> message : getMessages err
  WithLocation _ err -> getMessages err
  WithResponsibility _ err -> getMessages err

-- | @err@ must not have `ErrorAppend`.
getErrorGroup :: Error -> Maybe ErrorGroup
getErrorGroup = \case
  Error _ -> Nothing
  ErrorAppend _ _ -> bug "ErrorAppend is not allowed here."
  WithGroup group _ -> Just group
  WithWrapped _ err -> getErrorGroup err
  WithLocation _ err -> getErrorGroup err
  WithResponsibility _ err -> getErrorGroup err

-- | @err@ must not have `ErrorAppend`.
getLocation :: Error -> Maybe Loc
getLocation = \case
  Error _ -> Nothing
  ErrorAppend _ _ -> bug "ErrorAppend is not allowed here."
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

-- | @err@ must not have `ErrorAppend`.
getResponsibility :: Error -> Maybe Responsibility
getResponsibility = \case
  Error _ -> Nothing
  ErrorAppend _ _ -> bug "ErrorAppend is not allowed here."
  WithGroup group err -> case getResponsibility err of
    Just resp -> Just resp
    Nothing -> getResponsibilityFromErrorGroup group
  WithWrapped _ err -> getResponsibility err
  WithLocation _ err -> getResponsibility err
  WithResponsibility resp _ -> Just resp

hPrintError :: Handle -> Error -> IO ()
hPrintError handle err = do
  color <- hGetColorFlag handle
  mapM_ (hPutStrLn handle) (prettyError color err)

hPrintErrorWithText :: Handle -> Text -> Error -> IO ()
hPrintErrorWithText handle text err = do
  color <- hGetColorFlag handle
  mapM_ (hPutStrLn handle) (prettyErrorWithText color text err)
