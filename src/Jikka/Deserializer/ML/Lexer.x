{
module Jikka.Deserializer.ML.Lexer
    ( Token(..)
    , run
    ) where

import Jikka.Deserializer.ML.Pos
}

%wrapper "monad"
tokens :-

    $white+         ;
    "(*"            { begin comment }
    <comment> .     ;
    <comment> "*)"  { begin 0 }

    "()"            { tok' Unit }
    [ 0-9 ] +       { tok (Int . read) }
    "true"          { tok' (Bool True) }
    "false"         { tok' (Bool False) }

    "let"           { tok' Let }
    "rec"           { tok' Rec }
    "in"            { tok' In }
    "fun"           { tok' Fun }
    "function"      { tok' Function }
    "match"         { tok' Match }
    "with"          { tok' With }
    "end"           { tok' End }
    "if"            { tok' If }
    "then"          { tok' Then }
    "else"          { tok' Else }
    "given"         { tok' Given }

    "->"            { tok' Arrow }
    "|"             { tok' Bar }
    ":"             { tok' Colon }
    ","             { tok' Comma }
    "="             { tok' Equal }
    ";"             { tok' Semicolon }
    "_"             { tok' Underscore }

    "{"             { tok' OpenBrace }
    "["             { tok' OpenBracket }
    "("             { tok' OpenParen }
    "}"             { tok' CloseBrace }
    "]"             { tok' CloseBracket }
    ")"             { tok' CloseParen }

    [ A-Z a-z ] [ \' 0-9 A-Z a-z ] *  { tok Ident }
    [ \+ \- \* \/ \% \< \= \> \& \| \^ \@ ] +  { tok Op }

{
alexEOF :: Alex (Maybe (WithPos Token))
alexEOF = return $ Nothing

tok :: (String -> Token) -> AlexAction (Maybe (WithPos Token))
tok f (AlexPn _ line column, _, _, s) n = return . Just $ WithPos
    { value = f (take n s)
    , pos = Pos
        { line = line
        , column = column
        }
    }

tok' :: Token -> AlexAction (Maybe (WithPos Token))
tok' token = tok (const token)

data Token
    -- literals
    = Unit
    | Int Integer
    | Bool Bool
    -- keywords
    | Let
    | Rec
    | In
    | Fun
    | Function
    | Match
    | With
    | End
    | If
    | Then
    | Else
    | Given
    -- punctuations
    | Arrow
    | Bar
    | Colon
    | Comma
    | Equal
    | Semicolon
    | Underscore
    -- parens
    | OpenBrace
    | OpenBracket
    | OpenParen
    | CloseBrace
    | CloseBracket
    | CloseParen
    -- identifier
    | Ident String
    | Op String
    deriving (Eq, Ord, Show, Read)

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM f = do
    x <- f
    case x of
        Nothing -> return []
        Just x -> (x :) <$> unfoldM f

run :: String -> Either String [WithPos Token]
run input = runAlex input (unfoldM alexMonadScan)
}
