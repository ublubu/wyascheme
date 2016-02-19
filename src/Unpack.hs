{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Unpack where

import Control.Monad.Except
import Data.Either.Combinators

import Types

data Unpacker m = forall a. Eq a => AnyUnpacker (LispVal -> m a)

laxUnpackers :: (MonadError LispError m) => [Unpacker m]
laxUnpackers = [ AnyUnpacker unpackNum
               , AnyUnpacker extractBool
               , AnyUnpacker unpackString ]

strictUnpackers :: (MonadError LispError m) => [Unpacker m]
strictUnpackers = [ AnyUnpacker extractNum
                  , AnyUnpacker extractBool
                  , AnyUnpacker extractString ]

unpackEquals :: LispVal -> LispVal -> Unpacker (Except LispError) -> Bool
unpackEquals x y (AnyUnpacker unpacker) =
  fromRight False . runExcept $ do
  ux <- unpacker x
  uy <- unpacker y
  return $ ux == uy

unpackString :: (MonadError LispError m) => LispVal -> m String
unpackString (String s) = return s
unpackString (Number n) = return $ show n
unpackString (Bool b) = return $ show b
unpackString arg = throwError $ TypeMismatch "string" arg

unpackNum :: (MonadError LispError m) => LispVal -> m Integer
unpackNum (Number n) = return n
unpackNum arg@(String s) =
  case reads s of ((n,_):_) -> return n
                  _ -> throwError $ TypeMismatch "number" arg
unpackNum (List [n]) = unpackNum n
unpackNum arg = throwError $ TypeMismatch "number" arg

extractNum :: (MonadError LispError m) => LispVal -> m Integer
extractNum (Number n) = return n
extractNum nonNum = throwError $ TypeMismatch "number" nonNum

extractBool :: (MonadError LispError m) => LispVal -> m Bool
extractBool (Bool b) = return b
extractBool nonBool = throwError $ TypeMismatch "bool" nonBool

extractString :: (MonadError LispError m) => LispVal -> m String
extractString (String s) = return s
extractString nonString = throwError $ TypeMismatch "string" nonString
