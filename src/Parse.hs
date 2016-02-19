module Parse where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  void $ char '"'
  x <- many (noneOf "\"")
  void $ char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  init' <- endBy parseExpr spaces
  last' <- char '.' >> spaces >> parseExpr
  return $ DottedList init' last'

parseQuoted :: Parser LispVal
parseQuoted = do
  void $ char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do void $ char '('
         x <- try parseList <|> parseDottedList
         void $ char ')'
         return x
