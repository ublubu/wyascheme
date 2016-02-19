{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Except
import Text.ParserCombinators.Parsec (parse)
import Types
import Parse
import Eval

readExpr :: (MonadError LispError m) => String -> m LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError . Parser $ err
  Right val -> return val

test :: String -> IO ()
test = print . runExcept . (eval <=< readExpr)

main :: IO ()
main = putStrLn "hello"
