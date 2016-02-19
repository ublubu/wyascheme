{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Except
import Data.Either.Combinators
import System.IO

import Types
import Parse
import Eval

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO (Either LispError LispVal)
evalString env = runExceptT . (eval env <=< readExpr)

evalToString :: Env -> String -> IO String
evalToString env = fmap (uneither . mapBoth show show) . evalString env

uneither :: Either a a -> a
uneither (Left x) = x
uneither (Right x) = x

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env = putStrLn <=< evalToString env

repl :: Env -> IO ()
repl env = do
  expr <- readPrompt "Lisp>>> "
  case expr of "quit" -> return ()
               _ -> evalAndPrint env expr >> repl env

main :: IO ()
main = repl =<< primitiveBindings
