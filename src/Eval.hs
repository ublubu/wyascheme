 {-# LANGUAGE FlexibleContexts #-}
module Eval where

import Control.Monad.Except
import Types

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
