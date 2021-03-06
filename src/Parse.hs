{-# LANGUAGE FlexibleContexts #-}
module Parse
( readExpr
, readManyExpr
) where

import Control.Monad.Except

import Text.ParserCombinators.Parsec hiding (spaces)

import MalVal

readExpr :: (MonadError MalError m) => String -> m MalVal
readExpr = parseString malval

readManyExpr :: (MonadError MalError m) => String -> m [MalVal]
readManyExpr = parseString $ many (malval <* spaces)

parseString :: (MonadError MalError m) => Parser a -> String -> m a
parseString parser input = case parse line "" input of
    Left err -> throwError $ Parser err
    Right val -> return val
  where
    line = spaces *> parser <* spaces <* eof

malval :: Parser MalVal
malval = choice
  [ malMap
  , malSymbol
  , malNumber
  , malString
  , malList
  , specialForm
  ]

specialForm :: Parser MalVal
specialForm = choice $ map (uncurry2 sigil) sigils
  where uncurry2 f (a, b) = f a b
        sigils = [("@", "deref"), ("~@", "splice-unquote"), ("'", "quote"), ("`", "quasiquote"), ("~", "unquote")]

sigil :: String -> String -> Parser MalVal
sigil s sym = try $ do
                    string s
                    val <- malval
                    return $ List [Symbol sym, val]

malList :: Parser MalVal
malList = List <$> malCollection '(' ')'

malMap :: Parser MalVal
malMap = fmap (List . (:) (Symbol "hash-map")) elements
  where
    elements = malCollection '{' '}'

malCollection c1 c2 = char c1 *> spaces *> many (malval <* spaces) <* char c2

malString :: Parser MalVal
malString = do char '"'
               x <- many chars
               char '"'
               return $ String x
  where chars = escaped <|> noneOf "\""
        escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
        codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
        replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

escapedChar :: Char -> Char -> Parser Char
escapedChar code replacement = char code >> return replacement

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?^_#"

malSymbol :: Parser MalVal
malSymbol = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let sym = first:rest
               return $ case sym of
                 "true"  -> Bool True
                 "false" -> Bool False
                 "nil"   -> Nil
                 _       -> Symbol sym

malNumber :: Parser MalVal
malNumber = (Number . read) <$> many1 digit

spaces = skipMany (space <|> char ',')

eol :: Parser ()
eol = oneOf "\n\r" *> return ()
