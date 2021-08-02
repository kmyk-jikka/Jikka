{
-- vim: filetype=haskell
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Jikka.Core.Parse.Alex
-- Description : tokenizes the code of our core language with Alex.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Core.Parse.Alex
    ( run
    ) where

import Jikka.Common.Error
import Jikka.Common.Location
import Jikka.Core.Parse.Token
}

%wrapper "monad"

$space = [\ \t\n\r]

$alpha = [A-Z a-z]
$alnum = [0-9 A-Z a-z]
$doublequote = ["]
$backslash = [\\]
@nl = "\n" | "\r\n"

$digit = [0-9]
$nonzerodigit = [1-9]
$bindigit = [0-1]
$octdigit = [0-7]
$hexdigit = [0-9a-fA-F]

$shortstringchar_single = [^ \\ \r \n ']
$shortstringchar_double = [^ \\ \r \n ']
@stringescapeseq = $backslash .

tokens :-

    $space +        ;
    "--" [^ \r\n] * ;

    "true"          { tok (Bool True) }
    "false"         { tok (Bool False) }

    "0" ("_" ? "0") *                   { tok' parseInt }
    $nonzerodigit ("_" ? $digit) *      { tok' parseInt }
    "0" [bB] ("_" ? $bindigit) +        { tok' parseInt }
    "0" [oO] ("_" ? $octdigit) +        { tok' parseInt }
    "0" [xX] ("_" ? $hexdigit) +        { tok' parseInt }

    "let"           { tok Let }
    "rec"           { tok Rec }
    "in"            { tok In }
    "fun"           { tok Fun }
    "if"            { tok If }
    "then"          { tok Then }
    "else"          { tok Else }

    -- punctuations
    "->"            { tok Arrow }
    "="             { tok Equal }
    ":"             { tok Colon }
    ","             { tok Comma }
    "_"             { tok Underscore }
    "'"             { tok SingleQuote }
    "<-"            { tok BackArrow }

    -- parens
    "["             { tok OpenBracket }
    "("             { tok OpenParen }
    "]"             { tok CloseBracket }
    ")"             { tok CloseParen }

    -- arithmetic operators
    "+"             { tok (Operator Plus) }
    "-"             { tok (Operator Minus) }
    "*"             { tok (Operator Mult) }
    "/"             { tok (Operator FloorDiv) }
    "%"             { tok (Operator FloorMod) }
    "/^"            { tok (Operator CeilDiv) }
    "%^"            { tok (Operator CeilMod) }
    "**"            { tok (Operator Pow) }

    -- boolean operators
    "and"           { tok (Operator And) }
    "or"            { tok (Operator Or) }
    "not"           { tok (Operator Not) }
    "implies"       { tok (Operator Implies) }

    -- bit operators
    "~"             { tok (Operator BitNot) }
    "&"             { tok (Operator BitAnd) }
    "|"             { tok (Operator BitOr) }
    "^"             { tok (Operator BitXor) }
    "<<"            { tok (Operator BitLShift) }
    ">>"            { tok (Operator BitRShift) }

    -- min max operators
    "<?"            { tok (Operator Min) }
    ">?"            { tok (Operator Max) }

    -- comparators
    ">"             { tok (Operator GreaterThan) }
    "<"             { tok (Operator LessThan) }
    "<="            { tok (Operator LessEqual) }
    ">="            { tok (Operator GreaterEqual) }
    "=="            { tok (Operator DoubleEqual) }
    "/="            { tok (Operator NotEqual) }

    -- identifier
    $alpha ($alnum | "_" | ".") *                 { tok' Ident }
    $alpha ($alnum | "_" | ".") * "$" $digit +    { tok' Ident }

    -- catch error
    .               { skip' }
{
type Token'' = Either Error Token'

alexEOF :: Alex (Maybe Token'')
alexEOF = return Nothing

tok'' :: (Loc -> String -> Token'') -> AlexAction (Maybe Token'')
tok'' f (AlexPn _ line column, _, _, s) n = return . Just $ f loc (take n s) where
  loc = Loc
    { line = line
    , column = column
    , width = n
    }

tok' :: (String -> Token) -> AlexAction (Maybe Token'')
tok' f = tok'' (\loc s -> Right (WithLoc loc (f s)))

tok :: Token -> AlexAction (Maybe Token'')
tok token = tok' (const token)

parseInt :: String -> Token
parseInt s' = Int $ case filter (/= '_') s' of
  '0' : 'b' : s -> foldl (\acc c -> acc * 2 + read [c]) 0 (reverse s)
  '0' : 'B' : s -> foldl (\acc c -> acc * 2 + read [c]) 0 (reverse s)
  s@('0' : 'o' : _) -> read s
  s@('0' : 'O' : _) -> read s
  s@('0' : 'x' : _) -> read s
  s@('0' : 'X' : _) -> read s
  s -> read s

skip' :: AlexAction (Maybe Token'')
skip' (AlexPn _ line column, _, _, s) n = return (Just (Left err)) where
  loc = Loc line column n
  msg = show (take n s) ++ " is not a acceptable character"
  err = lexicalErrorAt loc msg

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM f = do
    x <- f
    case x of
        Nothing -> return []
        Just x -> (x :) <$> unfoldM f

run :: MonadError Error m => String -> m [Token']
run input = wrapError' "Jikka.Core.Parse.Alex" $ do
    case runAlex input (unfoldM alexMonadScan) of
      Left err -> throwInternalError $ "Alex says: " ++ err
      Right tokens -> reportErrors tokens
}
