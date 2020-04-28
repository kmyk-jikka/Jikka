{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Jikka.Deserializer.ML.Lexer
  ( Token (..),
    WithPos (..),
    TokenStream (..),
    lexer,
  )
where

import Data.List (uncons)
import Data.Proxy (Proxy (..))
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
        length' :: Int,
        value :: a
      }
  deriving (Eq, Ord, Show, Read, Functor)

newtype TokenStream = TokenStream {unTokenStream :: [WithPos Token]}
  deriving (Eq, Ord, Show, Read)

instance P.Stream TokenStream where
  type Token TokenStream = Token
  type Tokens TokenStream = [Token]
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  take1_ (TokenStream s) = case s of
    [] -> Nothing
    (token : tokens) -> Just (value token, TokenStream tokens)
  takeN_ n (TokenStream s) = case s of
    _ | n <= 0 -> Just ([], TokenStream s)
    [] -> Nothing
    _ -> let (ls, rs) = splitAt n s in Just (map value ls, TokenStream rs)
  takeWhile_ pred (TokenStream s) = let (ls, rs) = span (pred . value) s in (map value ls, TokenStream rs)
  showTokens Proxy = show
  chunkEmpty Proxy = null
  reachOffset offset pst =
    let (TokenStream tokens) = P.pstateInput pst
     in let tokens' = drop offset tokens'
         in let sourcePos
                  | not $ null tokens' = start $ head tokens'
                  | not $ null tokens = end $ last tokens
                  | otherwise = P.pstateSourcePos pst
             in let pst' =
                      pst
                        { P.pstateInput = TokenStream tokens',
                          P.pstateOffset = P.pstateOffset pst + offset,
                          P.pstateSourcePos = sourcePos
                        }
                 in let line = case tokens' of
                          [] -> "<eof>"
                          (token : _) -> show token
                     in (line, pst')

type Parser = P.Parsec Void Text

withPos :: Parser a -> Parser (WithPos a)
withPos f = do
  start <- P.getSourcePos
  offset <- P.getOffset
  value <- f
  end <- P.getSourcePos
  length'' <- subtract offset <$> P.getOffset
  return $
    WithPos
      { start = start,
        end = end,
        length' = length'',
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
  head <- P.try ((: []) <$> token1) <|> ([] <$ space)
  tail <- P.many token1 <* P.eof
  return . TokenStream $ head ++ tail
