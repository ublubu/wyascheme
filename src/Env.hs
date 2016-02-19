{-# LANGUAGE FlexibleContexts #-}

module Env where

import Control.Monad.Except
import Data.IORef
import Data.Maybe

import Types

nullEnv :: (MonadIO m) => m Env
nullEnv = liftIO $ newIORef []

isBound :: (MonadIO m) => Env -> String -> m Bool
isBound envRef var = do
   env <- liftIO $ readIORef envRef
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

bindVars :: (MonadIO m)
         => Env
         -> [(String, LispVal)]
         -> m Env
bindVars envRef bindings = liftIO $ do
  env <- readIORef envRef
  -- TODO: do something about binding to already-bound variables
  env' <- (++ env) <$> mapM addBinding bindings
  newIORef env'
  where addBinding (var, val) = (\r -> (var, r)) <$> newIORef val
