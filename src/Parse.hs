module Parse
( readExpr
) where

import MalVal

import Text.Parsec (try)
import Text.ParserCombinators.Parsec hiding (try, spaces)

import Control.Monad.Except

readExpr :: String -> MalResult
readExpr input = case parse parseExpr "" input of
  Left err -> (throwError . Parser) err
  Right val -> return val

parseExpr :: Parser MalVal
parseExpr =
  parseSymbol
  <|> parseString
  <|> parseNumber
  <|> do
    char '('
    parsePair

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

parseString :: Parser MalVal
parseString = do char '"'
                 x <- many chars
                 char '"'
                 return $ String x
  where chars = escaped <|> noneOf "\""
        escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
        codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
        replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

escapedChar :: Char -> Char -> Parser Char
escapedChar code replacement = char code >> return replacement

parseSymbol :: Parser MalVal
parseSymbol = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let sym = first:rest
  return $ case sym of
    "true"  -> Bool True
    "false" -> Bool False
    "nil"   -> Nil
    _       -> Symbol sym

parseNumber :: Parser MalVal
parseNumber = (Number . read) <$> many1 digit

skipSpaces = skipMany space

parsePair = do
  skipSpaces
  try $ char ')' >> return EmptyList
  <|> do
    car <- parseExpr
    skipSpaces
    try (parseDotted car) <|> parseList car

parseList car = do
  cdr <- parsePair
  return $ Pair car cdr

parseDotted car = do
  char '.'
  skipSpaces
  cdr <- parseExpr
  char ')'
  return $ Pair car cdr
