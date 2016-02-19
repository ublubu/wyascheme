{-# LANGUAGE RecordWildCards #-}

module Types where

import Control.Monad.Except
import Text.ParserCombinators.Parsec (ParseError)
import Data.IORef
import System.IO

type Env = IORef [(String, IORef LispVal)]

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> Either LispError LispVal)
             | IOFunc ([LispVal] -> ExceptT LispError IO LispVal)
             | Func { _funcParams :: [String]
                    , _funcVararg :: Maybe String
                    , _funcBody :: [LispVal]
                    , _funcClosure :: Env
                    }
             | Port Handle

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList init' last') = "(" ++ unwordsList init' ++ " . " ++ showVal last' ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
showVal Func{..} =
  "(lambda (" ++ unwords (map show _funcParams) ++
  maybe "" (\arg -> " . " ++ arg) _funcVararg ++ ") ...)"
showVal (Port _) = "<IO port>"

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String LispVal
               | UnboundVar String String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ "; found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

unwordsList :: [LispVal] -> String
unwordsList = unwords . fmap showVal

liftEither :: (MonadError e m) => Either e a -> m a
liftEither (Right x) = return x
liftEither (Left x) = throwError x

liftPrim :: (MonadError e m, MonadIO m)
         => ExceptT e IO a
         -> m a
liftPrim = liftEither <=< (liftIO . runExceptT)
