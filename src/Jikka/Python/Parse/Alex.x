{
-- vim: filetype=haskell
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Jikka.Core.Parse.Alex
-- Description : tokenizes the code of the standard Python with Alex.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- *   TODO: tokenize float literals
-- *   TODO: tokenize string literals
module Jikka.Python.Parse.Alex
    ( run
    ) where

import Data.Char (chr, isHexDigit, isOctDigit)
import Jikka.Common.Error
import Jikka.Common.Location
import Jikka.Common.Parse.JoinLines (joinLinesWithParens, removeEmptyLines)
import Jikka.Common.Parse.OffsideRule (insertIndents)
import Jikka.Python.Parse.Token
}

%wrapper "monad"

$space = [\ ]
$tab = [\t]

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
    "#" .*          ;
    $backslash @nl  ;
    @nl             { tok Newline }
    [\n\r]          { tok Newline }

    "None"          { tok None }
    "True"          { tok (Bool True) }
    "False"         { tok (Bool False) }

    "0" ("_" ? "0") *                   { tok' parseInt }
    $nonzerodigit ("_" ? $digit) *      { tok' parseInt }
    "0" [bB] ("_" ? $bindigit) +        { tok' parseInt }
    "0" [oO] ("_" ? $octdigit) +        { tok' parseInt }
    "0" [xX] ("_" ? $hexdigit) +        { tok' parseInt }

    "'" ($shortstringchar_single | @stringescapeseq) * "'"  { tok'' parseString }
    $doublequote ($shortstringchar_double | @stringescapeseq) * $doublequote  { tok'' parseString }

    "def"           { tok Def }
    "if"            { tok If }
    "elif"          { tok Elif }
    "else"          { tok Else }
    "for"           { tok For }
    "in"            { tok In }
    "assert"        { tok Assert }
    "return"        { tok Return }
    "lambda"        { tok Lambda }

    -- punctuations
    "->"            { tok Arrow }
    ":"             { tok Colon }
    ";"             { tok Semicolon }
    ","             { tok Comma }
    "."             { tok Dot }
    "="             { tok Equal }
    "_"             { tok Underscore }

    -- parens
    "{"             { tok OpenBrace }
    "["             { tok OpenBracket }
    "("             { tok OpenParen }
    "}"             { tok CloseBrace }
    "]"             { tok CloseBracket }
    ")"             { tok CloseParen }

    -- special operators
    "-"             { tok MinusOp }
    "*"             { tok MulOp }
    "**"            { tok PowOp }

    -- expr operators
    "+"             { tok PlusOp }
    "//"            { tok (DivModOp FloorDiv) }
    "/"             { tok (DivModOp Div) }
    "%"             { tok (DivModOp FloorMod) }
    "&"             { tok BitAndOp }
    "|"             { tok BitOrOp }
    "^"             { tok BitXorOp }
    "~"             { tok BitNotOp }
    "<<"            { tok BitLShiftOp }
    ">>"            { tok BitRShiftOp }
    ">"             { tok (CmpOp GreaterThan) }
    "<"             { tok (CmpOp LessThan) }
    "<="            { tok (CmpOp LessEqual) }
    ">="            { tok (CmpOp GreaterEqual) }
    "=="            { tok (CmpOp DoubleEqual) }
    "!="            { tok (CmpOp NotEqual) }
    "and"           { tok AndOp }
    "or"            { tok OrOp }
    "not"           { tok NotOp }
    "@"             { tok AtOp }
    ":="            { tok WalrusOp }

    -- assignment operators
    "+="            { tok (AugOp AugAdd) }
    "-="            { tok (AugOp AugSub) }
    "*="            { tok (AugOp AugMul) }
    "@="            { tok (AugOp AugAt) }
    "/="            { tok (AugOp AugDiv) }
    "//="           { tok (AugOp AugFloorDiv) }
    "%="            { tok (AugOp AugFloorMod) }
    "%^="           { tok (AugOp AugCeilMod) }
    "**="           { tok (AugOp AugPow) }
    "<<="           { tok (AugOp AugBitRShift) }
    ">>="           { tok (AugOp AugBitLShift) }
    "&="            { tok (AugOp AugBitAnd) }
    "^="            { tok (AugOp AugBitXor) }
    "|="            { tok (AugOp AugBitOr) }

    -- additional operators
    "/^"            { tok (DivModOp CeilDiv) }
    "%^"            { tok (DivModOp CeilMod) }
    "<?"            { tok MinOp }
    ">?"            { tok MaxOp }
    "implies"       { tok ImpliesOp }
    "/^="           { tok (AugOp AugCeilDiv) }
    "<?="           { tok (AugOp AugMin) }
    ">?="           { tok (AugOp AugMax) }

    -- Python reserved
    "as"            { tok As }
    "async"         { tok Async }
    "await"         { tok Await }
    "break"         { tok Break }
    "class"         { tok Class }
    "continue"      { tok Continue }
    "del"           { tok Del }
    "except"        { tok Except }
    "finally"       { tok Finally }
    "from"          { tok From }
    "global"        { tok Global }
    "import"        { tok Import }
    "is"            { tok Is }
    "nonlocal"      { tok Nonlocal }
    "pass"          { tok Pass }
    "raise"         { tok Raise }
    "try"           { tok Try }
    "while"         { tok While }
    "with"          { tok With }
    "yield"         { tok Yield }

    -- identifier
    ($alpha | _) ($alnum | _) *         { tok' Ident }

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

parseString :: Loc -> String -> Token''
parseString loc s = WithLoc loc . String <$> go s where
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
run input = wrapError' "Jikka.Python.Parse.Alex failed" $ do
    tokens <- case runAlex input (unfoldM alexMonadScan) of
      Left err -> throwInternalError $ "Alex says: " ++ err
      Right tokens -> return tokens
    tokens <- reportErrors tokens
    tokens <- joinLinesWithParens (`elem` [OpenParen, OpenBracket, OpenBrace]) (`elem` [CloseParen, CloseBracket, CloseBrace]) (== Newline) tokens
    tokens <- return $ removeEmptyLines (== Newline) tokens
    tokens <- insertIndents Indent Dedent (== Newline) tokens
    return tokens
}
