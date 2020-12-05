{
{-# LANGUAGE FlexibleContexts #-}

-- vim: filetype=haskell
module Jikka.Python.Parse.Alex
    ( Token(..)
    , run
    ) where

import Text.Read (readMaybe)
import Jikka.Common.Location
import Jikka.Common.Error
import Jikka.Common.Parse.OffsideRule (insertIndentTokens, IndentSetting(..))
}

%wrapper "monad"

$nl = [\n\r]
$space = [\ ]
$tab = [\t]

$digit = [0-9]
$alpha = [A-Z a-z]
$alnum = [0-9 A-Z a-z]
$doublequote = ["]

tokens :-

    $space +        ;
    $nl $white *    { tok' Newline }
    "#" . * $white * { tok' Newline }

    "None"          { tok' None }
    $digit +        { tok (Int . read) }
    "True"          { tok' (Bool True) }
    "False"         { tok' (Bool False) }

    "def"           { tok' Def }
    "if"            { tok' If }
    "elif"          { tok' Elif }
    "else"          { tok' Else }
    "for"           { tok' For }
    "in"            { tok' In }
    "assert"        { tok' Assert }
    "return"        { tok' Return }
    "import"        { tok' Import }
    "from"          { tok' From }

    -- punctuations
    "->"            { tok' Arrow }
    ":"             { tok' Colon }
    ","             { tok' Comma }
    "."             { tok' Dot }
    "="             { tok' Equal }
    "_"             { tok' Underscore }

    -- parens
    "{"             { tok' OpenBrace }
    "["             { tok' OpenBracket }
    "("             { tok' OpenParen }
    "}"             { tok' CloseBrace }
    "]"             { tok' CloseBracket }
    ")"             { tok' CloseParen }
    "'"             { tok' SingleQuote }
    $doublequote    { tok' DoubleQuote }

    $alpha [$alnum _] *  { tok Ident }

    -- Python operators
    "+"             { tok Op }
    "-"             { tok Op }
    "*"             { tok Op }
    "//"            { tok Op }
    "%"             { tok Op }
    "**"            { tok Op }
    "&"             { tok Op }
    "|"             { tok Op }
    "^"             { tok Op }
    "~"             { tok Op }
    "<<"            { tok Op }
    ">>"            { tok Op }
    ">"             { tok Op }
    "<"             { tok Op }
    "<="            { tok Op }
    ">="            { tok Op }
    "=="            { tok Op }
    "!="            { tok Op }
    "and"           { tok Op }
    "or"            { tok Op }
    "not"           { tok Op }

    -- additional operators
    "/^"            { tok Op }
    "<?"            { tok Op }
    ">?"            { tok Op }
    "implies"       { tok Op }

    -- Python reserved
    "as"            { reservedError }
    "async"         { reservedError }
    "await"         { reservedError }
    "break"         { reservedError }
    "class"         { reservedError }
    "continue"      { reservedError }
    "del"           { reservedError }
    "except"        { reservedError }
    "finally"       { reservedError }
    "global"        { reservedError }
    "is"            { reservedError }
    "lambda"        { reservedError }
    "nonlocal"      { reservedError }
    "pass"          { reservedError }
    "raise"         { reservedError }
    "try"           { reservedError }
    "while"         { reservedError }
    "with"          { reservedError }
    "yield"         { reservedError }

    -- catch error
    .               { skip' }
{
alexEOF :: Alex (Maybe (WithLoc Token))
alexEOF = return $ Nothing

tok :: (String -> Token) -> AlexAction (Maybe (WithLoc Token))
tok f (AlexPn _ line column, _, _, s) n = return . Just $ WithLoc
    { value = f (take n s)
    , loc = Loc
        { line = line
        , column = column
        , width = n
        }
    }

tok' :: Token -> AlexAction (Maybe (WithLoc Token))
tok' token = tok (const token)

data Token
    -- literals
    = None
    | Int Integer
    | Bool Bool
    -- keywords
    | Def
    | If
    | Elif
    | Else
    | For
    | In
    | Assert
    | Return
    | Import
    | From
    -- punctuations
    | Arrow
    | Colon
    | Comma
    | Dot
    | Equal
    | Underscore
    -- parens
    | OpenBrace
    | OpenBracket
    | OpenParen
    | CloseBrace
    | CloseBracket
    | CloseParen
    | SingleQuote
    | DoubleQuote
    -- identifier
    | Ident String
    | Op String
    -- indent
    | Newline
    | Indent
    | Dedent
    deriving (Eq, Ord, Show, Read)

reservedError :: AlexAction a
reservedError (AlexPn _ line column, _, _, s) n = alexError (show err) where
  msg = show (take n s) ++ " is a reserved Python keyword"
  err = (line, column, n, msg)

skip' :: AlexAction a
skip' (AlexPn _ line column, _, _, s) n = alexError (show err) where
  msg = show (take n s) ++ " is not a acceptable character"
  err = (line, column, n, msg)

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM f = do
    x <- f
    case x of
        Nothing -> return []
        Just x -> (x :) <$> unfoldM f

run :: MonadError Error m => String -> m [WithLoc Token]
run input = wrapError' "Jikka.Python.Parse.Alex failed" $ do
    tokens <- case runAlex input (unfoldM alexMonadScan) of
      Left err -> case readMaybe err of
        Nothing -> throwInternalError ("failed to parse: " ++ show err)
        Just (line, column, width, msg) -> throwLexicalErrorAt (Loc line column width) msg
      Right tokens -> return tokens
    let setting = IndentSetting
            { indentToken = Indent
            , dedentToken = Dedent
            , initialLine = 1
            , initialColumn = 1
            , isOpenParenToken = (`elem` [OpenParen, OpenBracket, OpenBrace])
            , isCloseParenToken = (`elem` [CloseParen, CloseBracket, CloseBrace])
            , allowNoMatchingDedent = False
            }
    tokens <- insertIndentTokens setting tokens
    return tokens
}
