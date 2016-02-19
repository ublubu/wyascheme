{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList init' last') = "(" ++ unwordsList init' ++ " . " ++ showVal last' ++ ")"

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected" ++ expected ++ "; found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

unwordsList :: [LispVal] -> String
unwordsList = unwords . fmap showVal

readExpr :: (MonadError LispError m) => String -> m LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError . Parser $ err
  Right val -> return val

eval :: (MonadError LispError m) => LispVal -> m LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = apply func =<< mapM eval args
eval badForm = throwError $ BadSpecialForm "unrecognized special form" badForm

apply :: (MonadError LispError m) => String -> [LispVal] -> m LispVal
apply func args =
  maybe
  (throwError $ NotFunction "Unrecognized primitive function" func)
  ($ args) $ lookup func primitives

primitives :: (MonadError LispError m) => [(String, [LispVal] -> m LispVal)]
primitives = [ ("+", nb (+))
             , ("-", nb (-))
             , ("*", nb (*))
             , ("/", nb div)
             , ("mod", nb mod)
             , ("quotient", nb quot)
             , ("remainder", nb rem)
             , ("symbol?", typeTest isSymbol)
             , ("string?", typeTest isString)
             , ("number?", typeTest isNumber)
             ]
  where typeTest f = oneArg $ return . Bool . f
        nb = twoArg . numericBinop

oneArg :: (MonadError LispError m) => (LispVal -> m a) -> [LispVal] -> m a
oneArg f [x] = f x
oneArg _ xs = throwError $ NumArgs 1 xs

twoArg :: (MonadError LispError m) => (LispVal -> LispVal -> m a) -> [LispVal] -> m a
twoArg f [x, y] = f x y
twoArg _ xs = throwError $ NumArgs 2 xs

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _ = False

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _ = False

numericBinop :: (MonadError LispError m) => (Integer -> Integer -> Integer) -> LispVal -> LispVal -> m LispVal
numericBinop = wrapBinop unpackNum Number

wrapBinop :: (Applicative m) => (a -> m b) -> (c -> d) -> (b -> b -> c) -> a -> a -> m d
wrapBinop unwrap wrap f x y = fmap wrap $ f <$> unwrap x <*> unwrap y

unpackNum :: (MonadError LispError m) => LispVal -> m Integer
unpackNum (Number n) = return n
unpackNum nonNum = throwError $ TypeMismatch "number" nonNum

test :: String -> IO ()
test = print . runExcept . (eval <=< readExpr)

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

main :: IO ()
main = putStrLn "hello"
