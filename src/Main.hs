{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Except
import Data.Either.Combinators
import System.IO
import Text.ParserCombinators.Parsec (parse)

import Types
import Parse
import Eval

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

readExpr :: (MonadError LispError m) => String -> m LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError . Parser $ err
  Right val -> return val

evalString :: String -> Either LispError LispVal
evalString = runExcept . (eval <=< readExpr)

evalToString :: String -> String
evalToString = uneither . mapBoth show show . evalString

uneither :: Either a a -> a
uneither (Left x) = x
uneither (Right x) = x

evalAndPrint :: String -> IO ()
evalAndPrint = putStrLn . evalToString

repl :: IO ()
repl = do
  expr <- readPrompt "Lisp>>> "
  case expr of "quit" -> return ()
               _ -> evalAndPrint expr >> repl

main :: IO ()
main = repl
