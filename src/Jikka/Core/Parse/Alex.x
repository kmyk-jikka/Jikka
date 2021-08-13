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

import Data.Char (chr, isHexDigit, isOctDigit)
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

    $doublequote ($shortstringchar_double | @stringescapeseq) * $doublequote  { tok'' parseString }

    "let"           { tok Let }
    "rec"           { tok Rec }
    "in"            { tok In }
    "fun"           { tok Fun }
    "if"            { tok If }
    "then"          { tok Then }
    "else"          { tok Else }
    "assert"        { tok Assert }
    "forall"        { tok Forall }

    -- punctuations
    "->"            { tok Arrow }
    "="             { tok Equal }
    ":"             { tok Colon }
    ","             { tok Comma }
    "_"             { tok Underscore }
    "."             { tok Dot }
    "<-"            { tok BackArrow }
    "@"             { tok At }

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
    "&&"            { tok (Operator And) }
    "||"            { tok (Operator Or) }

    -- bit operators
    "~"             { tok (Operator BitNot) }
    "&"             { tok (Operator BitAnd) }
    "|"             { tok (Operator BitOr) }
    "^"             { tok (Operator BitXor) }
    "<<"            { tok (Operator BitLShift) }
    ">>"            { tok (Operator BitRShift) }

    -- comparators
    ">"             { tok (Operator GreaterThan) }
    "<"             { tok (Operator LessThan) }
    "<="            { tok (Operator LessEqual) }
    ">="            { tok (Operator GreaterEqual) }
    "=="            { tok (Operator DoubleEqual) }
    "/="            { tok (Operator NotEqual) }

    -- identifier
    $alpha ($alnum | "_") *                 { tok' Ident }
    $alpha ($alnum | "_") * "$" $digit +    { tok' Ident }
    "$" $digit +                            { tok' Ident }

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

-- | TODO: Make this compatible to Haskell. The current implementation is for Python.
parseString :: Loc -> String -> Token''
parseString loc s = WithLoc loc . String <$> go (tail (init s)) where
  go "" = Right ""
  go ('\\' : s) = case s of
    [] -> throwInternalErrorAt loc "invalid escape sequence"
    'a' : s -> ('\a' :) <$> go s
    'b' : s -> ('\b' :) <$> go s
    'f' : s -> ('\f' :) <$> go s
    'n' : s -> ('\n' :) <$> go s
    'r' : s -> ('\r' :) <$> go s
    't' : s -> ('\t' :) <$> go s
    'v' : s -> ('\v' :) <$> go s
    o1 : o2 : o3 : s | isOctDigit o1 && isOctDigit o2 && isOctDigit o3 -> (chr (read ("0o" ++ [o1, o2, o3])) :) <$> go s
    o1 : o2 : s | isOctDigit o1 && isOctDigit o2 -> (chr (read ("0o" ++ [o1, o2])) :) <$> go s
    o1 : s | isOctDigit o1 -> (chr (read ("0o" ++ [o1])) :) <$> go s
    'x' : h1 : h2 : s | isHexDigit h1 && isHexDigit h2 -> (chr (read ("0x" ++ [h1, h2])) :) <$> go s
    'x' : _ -> throwLexicalErrorAt loc "truncated \\xXX escape"
    c : s -> (c :) <$> go s
  go (c : s) = (c :) <$> go s

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
