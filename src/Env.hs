{-# LANGUAGE FlexibleContexts #-}

module Env where

import Control.Monad.Except
import Data.IORef
import Data.Maybe

import Types

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = do
   env <- readIORef envRef
   return . isJust $ lookup var env

getVar :: (MonadError LispError m, MonadIO m)
       => Env
       -> String
       -> m LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: (MonadError LispError m, MonadIO m)
       => Env
       -> String
       -> LispVal
       -> m LispVal
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  case lookup var env of
    Nothing -> throwError $ UnboundVar "Setting an unbound variable" var
    Just varRef -> liftIO $ writeIORef varRef val
  return val

defineVar :: (MonadIO m)
          => Env
          -> String
          -> LispVal
          -> m LispVal
defineVar envRef var val = liftIO $ do
  env <- readIORef envRef
  case lookup var env of
    Nothing -> do
      varRef <- newIORef val
      writeIORef envRef ((var, varRef):env)
    Just varRef -> writeIORef varRef val
  return val

--bindVars :: (MonadError LispError m, MonadIO m)
--         => Env
--         -> [(String, LispVal)]
--         -> IO Env
--bindVars envRef bindings = do
