module Snow.Parser (parseType, parseCommand, unsafeParseType) where

import Prelude

import Control.Lazy (fix)
import Control.Plus ((<|>))
import Data.Either (Either, fromRight)
import Data.Foldable (foldl, foldr)
import Data.Identity (Identity)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (maybe, optional)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Snow.Repl.Types (Command(..))
import Snow.Type (SnowType(..))
import Text.Parsing.Parser (ParseError, Parser, ParserT, fail, runParser)
import Text.Parsing.Parser.Combinators (many1, try)
import Text.Parsing.Parser.String (class StringLike, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, LanguageDef, alphaNum, letter, makeTokenParser)
import Undefined (undefined)

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
    , reservedOpNames: [ ".", "->", "\\", "::", ":", "*" ]
    , reservedNames: [ "Unit", "forall", "unit", "exists", "pi", "effect" ]
    , identStart: letter
    , identLetter: alphaNum <|> oneOf [ '_', '\'' ]
    }

tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser language

-- | Parser for types
parseExpression' :: Parser String SnowType -> Parser String SnowType
parseExpression' expr = parseAnnotation
  where
  { parens, identifier, natural, reserved, reservedOp } = tokenParser

  parseAnnotation = ado
    call <- parseCall
    type_ <- optional (reservedOp "::" *> expr)
    in maybe call (Annotation call) type_

  parseCall = do
    subExpressions <- many1 atom
    let calee = NonEmptyList.head subExpressions
    let arguments = NonEmptyList.tail subExpressions
    pure $ foldl Application calee arguments

  universal = Universal <$> identifier

  unitExpr = ExprUnit <$ reserved "unit"
  unitType = Unit <$ reserved "Unit"

  star = do
    explicit <- try (reservedOp "*/" $> true) <|> (reservedOp "*" $> false)
    level <- if not explicit then pure 0 else natural
    pure $ Star level

  atom = parens expr
    <|> star
    <|> unitExpr
    <|> unitType
    <|> lambda
    <|> parseForall
    <|> parseExists
    <|> parseFunction
    <|> universal

  lambda = ado
    arguments <- reservedOp "\\" *> many1 identifier
    body <- reservedOp "->" *> expr
    in foldr Lambda body arguments

  ------ Type binders
  parseFullDomain = parens do
    name <- identifier
    reservedOp ":"
    domain <- expr
    pure $ name /\ domain

  parseStarDomain = identifier <#> \name -> name /\ Star 0
  parseDomain = parseFullDomain <|> parseStarDomain

  parseFunction = do
    reserved "pi"
    var /\ domain <- parseFullDomain <|> (expr <#> (/\) "_")
    reservedOp "->"
    codomain <- expr
    pure (Pi var domain codomain)

  parseBinder = do
    vars <- many1 parseDomain
    reservedOp "."
    ty <- expr
    pure $ vars /\ ty

  parseForall = ado
    reserved "forall"
    vars /\ innerType <- parseBinder
    in foldr (uncurry Forall) innerType vars

  parseExists = ado
    reserved "exists"
    vars /\ innerType <- parseBinder
    in foldr (uncurry Exists) innerType vars

-- | Complete parsers
typeParser :: Parser String SnowType
typeParser = expressionParser

expressionParser :: Parser String SnowType
expressionParser = fix parseExpression'

commandParser :: Parser String Command
commandParser = reservedOp ":" *> do
  command <- identifier
  case command of
    "t" -> TypeOf <$> expressionParser
    "s" -> Subsumes <$> parens typeParser <*> typeParser
    "assume" -> Assume <$> identifier <*> typeParser
    other -> fail $ "Unknown command " <> other
  where
  { identifier, reservedOp, parens } = tokenParser

parseType :: String -> Either ParseError SnowType
parseType = flip runParser typeParser

parseCommand :: String -> Either ParseError Command
parseCommand = flip runParser commandParser

unsafeParseType :: String -> SnowType
unsafeParseType = parseType >>> fromRight undefined