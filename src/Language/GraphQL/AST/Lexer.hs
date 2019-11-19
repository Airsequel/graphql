{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines a bunch of small parsers used to parse individual
--   lexemes.
module Language.GraphQL.AST.Lexer 
    ( Parser
    , amp
    , at
    , bang
    , blockString
    , braces
    , brackets
    , colon
    , dollar
    , comment
    , equals
    , integer
    , float
    , lexeme
    , name
    , parens
    , pipe
    , spaceConsumer
    , spread
    , string
    , symbol
    , unicodeBOM
    ) where

import Control.Applicative ( Alternative(..)
                           , liftA2
                           )
import Data.Char ( chr
                 , digitToInt
                 , isAsciiLower
                 , isAsciiUpper
                 , ord
                 )
import Data.Foldable (foldl')
import Data.List (dropWhileEnd)
import Data.Proxy (Proxy(..))
import Data.Void (Void)
import Text.Megaparsec ( Parsec
                       , between
                       , chunk
                       , chunkToTokens
                       , notFollowedBy
                       , oneOf
                       , option
                       , optional
                       , satisfy
                       , sepBy
                       , skipSome
                       , takeP
                       , takeWhile1P
                       , try
                       )
import Text.Megaparsec.Char ( char
                            , digitChar
                            , space1
                            )
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- | Standard parser.
-- Accepts the type of the parsed token.
type Parser = Parsec Void T.Text

ignoredCharacters :: Parser ()
ignoredCharacters = space1 <|> skipSome (char ',')

-- | Parser that skips comments and meaningless characters, whitespaces and
-- commas.
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space ignoredCharacters comment empty

-- | Parser for comments.
comment :: Parser ()
comment = Lexer.skipLineComment "#"

-- | Lexeme definition which ignores whitespaces and commas.
lexeme :: forall a. Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- | Symbol definition which ignores whitespaces and commas.
symbol :: T.Text -> Parser T.Text
symbol = Lexer.symbol spaceConsumer

-- | Parser for "!".
bang :: Parser T.Text
bang = symbol "!"

-- | Parser for "$".
dollar :: Parser T.Text
dollar = symbol "$"

-- | Parser for "@".
at :: Parser Char
at = char '@'

-- | Parser for "&".
amp :: Parser T.Text
amp = symbol "&"

-- | Parser for ":".
colon :: Parser T.Text
colon = symbol ":"

-- | Parser for "=".
equals :: Parser T.Text
equals = symbol "="

-- | Parser for the spread operator (...).
spread :: Parser T.Text
spread = symbol "..."

-- | Parser for "|".
pipe :: Parser T.Text
pipe = symbol "|"

-- | Parser for an expression between "(" and ")".
parens :: forall a. Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parser for an expression between "[" and "]".
brackets :: forall a. Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Parser for an expression between "{" and "}".
braces :: forall a. Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parser for strings.
string :: Parser T.Text
string = between "\"" "\"" stringValue
  where
    stringValue = T.pack <$> many stringCharacter
    stringCharacter = satisfy isStringCharacter1
        <|> escapeSequence
    isStringCharacter1 = liftA2 (&&) isSourceCharacter isChunkDelimiter

-- | Parser for block strings.
blockString :: Parser T.Text
blockString = between "\"\"\"" "\"\"\"" stringValue
  where
    stringValue = do
        byLine <- sepBy (many blockStringCharacter) lineTerminator
        let indentSize = foldr countIndent 0 $ tail byLine
            withoutIndent = head byLine : (removeIndent indentSize <$> tail byLine)
            withoutEmptyLines = liftA2 (.) dropWhile dropWhileEnd removeEmptyLine withoutIndent

        return $ T.intercalate "\n" $ T.concat <$> withoutEmptyLines
    removeEmptyLine [] = True
    removeEmptyLine [x] = T.null x || isWhiteSpace (T.head x)
    removeEmptyLine _ = False
    blockStringCharacter
        = takeWhile1P Nothing isWhiteSpace
        <|> takeWhile1P Nothing isBlockStringCharacter1
        <|> escapeTripleQuote
        <|> try (chunk "\"" <* notFollowedBy (chunk "\"\""))
    escapeTripleQuote = chunk "\\" >>= flip option (chunk "\"\"")
    isBlockStringCharacter1 = liftA2 (&&) isSourceCharacter isChunkDelimiter
    countIndent [] acc = acc
    countIndent (x:_) acc
        | T.null x = acc
        | not (isWhiteSpace $ T.head x) = acc
        | acc == 0 = T.length x
        | otherwise = min acc $ T.length x
    removeIndent _ [] = []
    removeIndent n (x:chunks) = T.drop n x : chunks

-- | Parser for integers.
integer :: Integral a => Parser a
integer = Lexer.signed (pure ()) $ lexeme Lexer.decimal

-- | Parser for floating-point numbers.
float :: Parser Double
float = Lexer.signed (pure ()) $ lexeme Lexer.float

-- | Parser for names (/[_A-Za-z][_0-9A-Za-z]*/).
name :: Parser T.Text
name = do
    firstLetter <- nameFirstLetter
    rest <- many $ nameFirstLetter <|> digitChar
    _ <- spaceConsumer
    return $ TL.toStrict $ TL.cons firstLetter $ TL.pack rest
      where
        nameFirstLetter = satisfy isAsciiUpper <|> satisfy isAsciiLower <|> char '_'

isChunkDelimiter :: Char -> Bool
isChunkDelimiter = flip notElem ['"', '\\', '\n', '\r']

isWhiteSpace :: Char -> Bool
isWhiteSpace = liftA2 (||) (== ' ') (== '\t')

lineTerminator :: Parser T.Text
lineTerminator = chunk "\r\n" <|> chunk "\n" <|> chunk "\r"

isSourceCharacter :: Char -> Bool
isSourceCharacter = isSourceCharacter' . ord
  where
    isSourceCharacter' code = code >= 0x0020
                           || code == 0x0009
                           || code == 0x000a
                           || code == 0x000d

escapeSequence :: Parser Char
escapeSequence = do
    _ <- char '\\'
    escaped <- oneOf ['"', '\\', '/', 'b', 'f', 'n', 'r', 't', 'u']
    case escaped of
        'b' -> return '\b'
        'f' -> return '\f'
        'n' -> return '\n'
        'r' -> return '\r'
        't' -> return '\t'
        'u' -> chr . foldl' step 0
                   . chunkToTokens (Proxy :: Proxy T.Text)
                 <$> takeP Nothing 4
        _ -> return escaped
  where
    step accumulator = (accumulator * 16 +) . digitToInt

-- | Parser for the "Byte Order Mark".
unicodeBOM :: Parser ()
unicodeBOM = optional (char '\xfeff') >> pure ()
