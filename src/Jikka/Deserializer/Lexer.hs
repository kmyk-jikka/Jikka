{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Jikka.Deserializer.Lexer
  ( Token (..),
    WithPos (..),
    lexer,
    TokenStream (..),
  )
where

import Data.Text (Text, unpack)
import Data.Void (Void)
import qualified Text.Megaparsec as P
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

data Token
  = -- literals
    Unit
  | Int Integer
  | Bool Bool
  | -- keywords
    Let
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
  | -- punctuations
    Arrow
  | Bar
  | Colon
  | Comma
  | Equal
  | Semicolon
  | -- parens
    CloseBrace
  | CloseBracket
  | CloseParen
  | OpenBrace
  | OpenBracket
  | OpenParen
  | -- identifier
    Underscore
  | Ident String
  | Op String
  deriving (Eq, Ord, Show, Read)

data WithPos a
  = WithPos
      { start :: P.SourcePos,
        end :: P.SourcePos,
        value :: a
      }
  deriving (Eq, Ord, Show, Read, Functor)

data TokenStream
  = TokenStream
      { name :: FilePath,
        input :: Text,
        stream :: [WithPos Token]
      }

instance P.Stream TokenStream where
  type Token TokenStream = WithPos Token
  type Tokens TokenStream = [WithPos Token]

type Parser = P.Parsec Void Text

withPos :: Parser a -> Parser (WithPos a)
withPos f = do
  start <- P.getSourcePos
  value <- f
  end <- P.getSourcePos
  return $
    WithPos
      { start = start,
        end = end,
        value = value
      }

skipLineComment :: Parser ()
skipLineComment = L.skipLineComment "#"

skipBlockComment :: Parser ()
skipBlockComment = L.skipBlockCommentNested "(*" "*)"

space :: Parser ()
space = L.space P.space1 skipLineComment skipBlockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

token1 :: Parser (WithPos Token)
token1 =
  withPos $
    P.choice
      -- literals
      [ Unit <$ symbol "()", -- before "("
        Int <$> lexeme L.decimal,
        Int <$> P.try (lexeme (L.signed P.empty L.decimal)), -- before "+", "-"
        Bool True <$ symbol "true",
        Bool False <$ symbol "false",
        -- keywords
        Let <$ symbol "let",
        Rec <$ symbol "rec",
        In <$ symbol "in",
        Fun <$ symbol "fun",
        Function <$ symbol "function",
        Match <$ symbol "match",
        With <$ symbol "with",
        End <$ symbol "end",
        If <$ symbol "if",
        Then <$ symbol "then",
        Else <$ symbol "else",
        Given <$ symbol "given",
        Arrow <$ symbol "->", -- before "-"
            -- operators
        Op . unpack <$> symbol "**", -- before "*"
        Op . unpack <$> symbol "+",
        Op . unpack <$> symbol "-",
        Op . unpack <$> symbol "*",
        Op . unpack <$> symbol "/",
        Op . unpack <$> symbol "%",
        Op . unpack <$> symbol "==", -- before "="
        Op . unpack <$> symbol "<>", -- before "<"
        Op . unpack <$> symbol "<=", -- before "<"
        Op . unpack <$> symbol "<",
        Op . unpack <$> symbol ">=", -- before ">"
        Op . unpack <$> symbol ">",
        -- punctuations
        Bar <$ symbol "|",
        Colon <$ symbol ":",
        Comma <$ symbol ",",
        Equal <$ symbol "=",
        Semicolon <$ symbol ";",
        -- parens
        CloseBrace <$ symbol "{",
        CloseBracket <$ symbol "}",
        CloseParen <$ symbol "(",
        OpenBrace <$ symbol ")",
        OpenBracket <$ symbol "[",
        OpenParen <$ symbol "]",
        -- identifier
        Underscore <$ symbol "_",
        Ident <$> P.try (lexeme $ P.some P.alphaNumChar)
      ]

lexer :: Parser TokenStream
lexer = do
  name <- P.sourceName <$> P.getSourcePos
  input <- P.getInput
  stream <- do
    head <- P.try ((: []) <$> token1) <|> ([] <$ space)
    tail <- P.many token1 <* P.eof
    return $ head ++ tail
  return $
    TokenStream
      { name = name,
        input = input,
        stream = stream
      }
