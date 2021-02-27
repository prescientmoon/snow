module Parser (parseType, unsafeParseType) where

import Prelude

import Control.Lazy (fix)
import Control.Plus ((<|>))
import Data.Either (Either, fromRight)
import Data.Identity (Identity)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (ParseError, Parser, ParserT, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (class StringLike, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, LanguageDef, alphaNum, letter, makeTokenParser)
import Type (Type(..))

opChars :: forall s m. StringLike s => Monad m => ParserT s m Char
opChars = oneOf [ ':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~' ]

language :: LanguageDef
language =
  LanguageDef
    { commentStart: "{-"
    , commentEnd: "-}"
    , commentLine: "--"
    , nestedComments: true
    , opStart: opChars
    , opLetter: opChars
    , caseSensitive: true
    , reservedOpNames: [ ".", "->" ]
    , reservedNames: [ "Unit", "forall" ]
    , identStart: letter
    , identLetter: alphaNum <|> oneOf [ '_', '\'' ]
    }

tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser language

-- | Parser for types
parseType' :: Parser String Type -> Parser String Type
parseType' type' = (try function) <|> nonFunction
  where
  { parens, identifier, reserved, reservedOp } = tokenParser

  typeVar = Universal <$> identifier
  nonFunction = parens type' <|> unit <|> parseForall <|> typeVar
  unit = Unit <$ reserved "Unit"

  parseForall = do
    reserved "forall"
    var <- identifier
    reservedOp "."
    ty <- type'
    pure $ Forall var ty

  function = do
    from <- nonFunction
    reservedOp "->"
    to <- type'
    pure $ Function from to

-- | Parser for types
typeParser :: Parser String Type
typeParser = fix parseType'

parseType :: String -> Either ParseError Type
parseType = flip runParser typeParser

unsafeParseType :: String -> Type
unsafeParseType = unsafePartial $ parseType >>> fromRight 