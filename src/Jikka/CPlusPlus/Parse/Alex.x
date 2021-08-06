{
-- vim: filetype=haskell
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Jikka.CPlusPlus.Parse.Alex
-- Description : tokenizes programs of C++ language with Alex.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.CPlusPlus.Parse.Alex
    ( run
    ) where

import Data.Char (chr, isHexDigit, isOctDigit)
import Jikka.Common.Error
import Jikka.Common.Location
import Jikka.CPlusPlus.Parse.Token
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
    "//" [^\r\n] * ;
    "/*" ([^\*] | "*" + [^\*\/]) * "*/" ;

    -- ignore preprocessor directives
    "#" [^\r\n] * ;

    "true"          { tok (Bool True) }
    "false"         { tok (Bool False) }

    $nonzerodigit $digit * $integer_suffix     { tok' parseInt }
    "0" [bB] $bindigit + $integer_suffix       { tok' parseInt }
    "0" $octdigit * $integer_suffix            { tok' parseInt }
    "0" [xX] $hexdigit + $integer_suffix       { tok' parseInt }

    "'" ($shortstringchar_single | @stringescapeseq) "'"  { tok'' parseChar }
    $doublequote ($shortstringchar_double | @stringescapeseq) * $doublequote  { tok'' parseString }

    -- keywords https://en.cppreference.com/w/cpp/keyword
    "alignas"                   { tok (Keyword Alignas) }
    "alignof"                   { tok (Keyword Alignof) }
    "and"                       { tok (Keyword And) }
    "and_eq"                    { tok (Keyword AndEq) }
    "asm"                       { tok (Keyword Asm) }
    "atomic_cancel"             { tok (Keyword AtomicCancel) }
    "atomic_commit"             { tok (Keyword AtomicCommit) }
    "atomic_noexcept"           { tok (Keyword AtomicNoexcept) }
    "auto"                      { tok (Keyword Auto) }
    "bitand"                    { tok (Keyword Bitand) }
    "bitor"                     { tok (Keyword Bitor) }
    "bool"                      { tok (Keyword Bool') }
    "break"                     { tok (Keyword Break) }
    "case"                      { tok (Keyword Case) }
    "catch"                     { tok (Keyword Catch) }
    "char"                      { tok (Keyword Char') }
    "char8_t"                   { tok (Keyword Char8T) }
    "char16_t"                  { tok (Keyword Char16T) }
    "char32_t"                  { tok (Keyword Char32T) }
    "class"                     { tok (Keyword Class) }
    "compl"                     { tok (Keyword Compl) }
    "concept"                   { tok (Keyword Concept) }
    "const"                     { tok (Keyword Const) }
    "consteval"                 { tok (Keyword Consteval) }
    "constexpr"                 { tok (Keyword Constexpr) }
    "constinit"                 { tok (Keyword Constinit) }
    "const_cast"                { tok (Keyword ConstCast) }
    "continue"                  { tok (Keyword Continue) }
    "co_await"                  { tok (Keyword CoAwait) }
    "co_return"                 { tok (Keyword CoReturn) }
    "co_yield"                  { tok (Keyword CoYield) }
    "decltype"                  { tok (Keyword Decltype) }
    "default"                   { tok (Keyword Default) }
    "delete"                    { tok (Keyword Delete) }
    "do"                        { tok (Keyword Do) }
    "double"                    { tok (Keyword Double) }
    "dynamic_cast"              { tok (Keyword DynamicCast) }
    "else"                      { tok (Keyword Else) }
    "enum"                      { tok (Keyword Enum) }
    "explicit"                  { tok (Keyword Explicit) }
    "export"                    { tok (Keyword Export) }
    "extern"                    { tok (Keyword Extern) }
    "false"                     { tok (Keyword False') }
    "float"                     { tok (Keyword Float) }
    "for"                       { tok (Keyword For) }
    "friend"                    { tok (Keyword Friend) }
    "goto"                      { tok (Keyword Goto) }
    "if"                        { tok (Keyword If) }
    "inline"                    { tok (Keyword Inline) }
    "int"                       { tok (Keyword Int') }
    "long"                      { tok (Keyword Long') }
    "mutable"                   { tok (Keyword Mutable) }
    "namespace"                 { tok (Keyword Namespace) }
    "new"                       { tok (Keyword New) }
    "noexcept"                  { tok (Keyword Noexcept) }
    "not"                       { tok (Keyword Not) }
    "not_eq"                    { tok (Keyword NotEq) }
    "nullptr"                   { tok (Keyword Nullptr) }
    "operator"                  { tok (Keyword Operator') }
    "or"                        { tok (Keyword Or) }
    "or_eq"                     { tok (Keyword OrEq) }
    "private"                   { tok (Keyword Private) }
    "protected"                 { tok (Keyword Protected) }
    "public"                    { tok (Keyword Public) }
    "reflexpr"                  { tok (Keyword Reflexpr) }
    "register"                  { tok (Keyword Register) }
    "reinterpret_cast"          { tok (Keyword ReinterpretCast) }
    "requires"                  { tok (Keyword Requires) }
    "return"                    { tok (Keyword Return) }
    "short"                     { tok (Keyword Short) }
    "signed"                    { tok (Keyword Signed) }
    "sizeof"                    { tok (Keyword Sizeof) }
    "static"                    { tok (Keyword Static) }
    "static_assert"             { tok (Keyword StaticAssert) }
    "static_cast"               { tok (Keyword StaticCast) }
    "struct"                    { tok (Keyword Struct) }
    "switch"                    { tok (Keyword Switch) }
    "synchronized"              { tok (Keyword Synchronized) }
    "template"                  { tok (Keyword Template) }
    "this"                      { tok (Keyword This) }
    "thread_local"              { tok (Keyword ThreadLocal) }
    "throw"                     { tok (Keyword Throw) }
    "true"                      { tok (Keyword True') }
    "try"                       { tok (Keyword Try) }
    "typedef"                   { tok (Keyword Typedef) }
    "typeid"                    { tok (Keyword Typeid) }
    "typename"                  { tok (Keyword Typename) }
    "union"                     { tok (Keyword Union) }
    "unsigned"                  { tok (Keyword Unsigned) }
    "using"                     { tok (Keyword Using) }
    "virtual"                   { tok (Keyword Virtual) }
    "void"                      { tok (Keyword Void) }
    "volatile"                  { tok (Keyword Volatile) }
    "wchar_t"                   { tok (Keyword WcharT) }
    "while"                     { tok (Keyword While) }
    "xor"                       { tok (Keyword Xor) }
    "xor_eq"                    { tok (Keyword XorEq) }

    -- punctuations
    "&"             { tok Ampersand }
    "->"            { tok Arrow }
    ":"             { tok Colon }
    ","             { tok Comma }
    "."             { tok Dot }
    "::"            { tok DoubleColon }
    "="             { tok Equal }
    "?"             { tok Question }

    -- parens
    "["             { tok OpenBracket }
    "("             { tok OpenParen }
    "{"             { tok OpenBrace }
    "]"             { tok CloseBracket }
    ")"             { tok CloseParen }
    "}"             { tok CloseBrace }

    -- arithmetic operators
    "+"             { tok (Operator Plus) }
    "-"             { tok (Operator Minus) }
    "*"             { tok (Operator Mult) }
    "/"             { tok (Operator Div) }
    "%"             { tok (Operator Mod) }

    "!"             { tok (Operator LogicalNot) }
    "&&"            { tok (Operator LogicalAnd) }
    "||"            { tok (Operator LogicalOr) }

    -- augumented arithmetic operators
    "+="            { tok (Operator PlusAssign) }
    "-="            { tok (Operator MinusAssign) }
    "*="            { tok (Operator MultAssign) }
    "/="            { tok (Operator DivAssign) }
    "%="            { tok (Operator ModAssign) }

    -- bit operators
    "~"             { tok (Operator BitNot) }
    "&"             { tok (Operator BitAnd) }
    "|"             { tok (Operator BitOr) }
    "^"             { tok (Operator BitXor) }
    "<<"            { tok (Operator BitLShift) }
    ">>"            { tok (Operator BitRShift) }

    -- bit operators
    "&="            { tok (Operator BitAndAssign) }
    "|="            { tok (Operator BitOrAssign) }
    "^="            { tok (Operator BitXorAssign) }
    "<<="           { tok (Operator BitLShiftAssign) }
    ">>="           { tok (Operator BitRShiftAssign) }

    -- comparators
    ">"             { tok (Operator GreaterThan) }
    "<"             { tok (Operator LessThan) }
    "<="            { tok (Operator LessEqual) }
    ">="            { tok (Operator GreaterEqual) }
    "=="            { tok (Operator DoubleEqual) }
    "!="            { tok (Operator NotEqual) }

    -- additional keywords
    "int8_t"            { tok Int8T }
    "int16_t"           { tok Int16T }
    "int32_t"           { tok Int32T }
    "int64_t"           { tok Int64T }
    "uint8_t"           { tok UInt8T }
    "uint16_t"          { tok UInt16T }
    "uint32_t"          { tok UInt32T }
    "uint64_t"          { tok UInt64T }

    -- REP macros
    "REP"               { tok REP }
    "rep"               { tok REP }
    "REP3"              { tok REP3 }
    "FOR"               { tok REP3 }
    "REP_R"             { tok REP_R }
    "REPR"              { tok REP_R }
    "RREP"              { tok REP_R }
    "REP3R"             { tok REP3R }

    -- types in std::
    "std"               { tok Std }
    "array"             { tok Array }
    "deque"             { tok Deque }
    "list"              { tok List }
    "map"               { tok Map }
    "pair"              { tok Pair }
    "priority_queue"    { tok PriorityQueue }
    "queue"             { tok Queue }
    "set"               { tok Set }
    "string"            { tok String' }
    "tuple"             { tok Tuple }
    "unordered_map"     { tok UnorderedMap }
    "unordered_set"     { tok UnorderedSet }
    "vector"            { tok Vector }

    -- types in atcoder::
    "atcoder"           { tok AtCoder }
    "segtree"           { tok Segtree }

    -- types in jikka::
    "jikka"             { tok Jikka }
    "convex_hull_trick" { tok ConvexHullTrick }

    -- identifier
    $alpha ($alnum | "_") *                 { tok' Ident }
    $alpha ($alnum | "_") * "$" $digit +    { tok' Ident }

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
  s@('0' : 'x' : _) -> read s
  s@('0' : 'X' : _) -> read s
  '0' : s -> read ("0o" ++ s)
  s -> read s

parseChar :: Loc -> String -> Token''
parseChar loc s = do
  tok <- parseString loc s
  case value tok of
    String [c] -> return $ Char c <$ tok
    _ -> throwInternalErrorAt loc "wrong char literal"

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
run input = wrapError' "Jikka.CPlusPlus.Parse.Alex" $ do
    case runAlex input (unfoldM alexMonadScan) of
      Left err -> throwInternalError $ "Alex says: " ++ err
      Right tokens -> reportErrors tokens
}
