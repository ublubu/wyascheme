{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Eval where

import Control.Lens ((%~), each, _2, (&))
import Control.Monad.Except
import Data.Maybe
import System.IO

import Types
import Env
import Parse
import PrimitiveFunc

eval :: (MonadError LispError m, MonadIO m)
     => Env
     -> LispVal
     -> m LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom var) = getVar env var
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", test, true, false]) = do
  result <- eval env test
  case result of Bool False -> eval env false
                 Bool True -> eval env true
                 _ -> throwError $ TypeMismatch "bool" result
eval env (List [Atom "set!", Atom var, form]) =
  setVar env var =<< eval env form
eval env (List [Atom "define", Atom var, form]) =
  defineVar env var =<< eval env form
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  defineVar env var Func { _funcParams = fmap showVal params
                         , _funcVararg = Nothing
                         , _funcBody = body
                         , _funcClosure = env }
eval env (List (Atom "define" : DottedList (Atom var : params) vararg : body)) =
  defineVar env var Func { _funcParams = fmap showVal params
                         , _funcVararg = Just . showVal $ vararg
                         , _funcBody = body
                         , _funcClosure = env }
eval env (List (Atom "lambda" : List params : body)) =
  return Func { _funcParams = fmap showVal params
              , _funcVararg = Nothing
              , _funcBody = body
              , _funcClosure = env }
eval env (List (Atom "lambda" : DottedList params vararg : body)) =
  return Func { _funcParams = fmap showVal params
              , _funcVararg = Just . showVal $ vararg
              , _funcBody = body
              , _funcClosure = env }
eval env (List (Atom "lambda" : vararg@(Atom _) : body)) =
  return Func { _funcParams = []
              , _funcVararg = Just . showVal $ vararg
              , _funcBody = body
              , _funcClosure = env }
eval env (List [Atom "load", String filename]) =
  (fmap last . mapM (eval env)) =<< load filename
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "unrecognized special form" badForm

ioPrimitives :: (MonadError LispError m, MonadIO m) => [(String, [LispVal] -> m LispVal)]
ioPrimitives = [ ("apply", applyProc)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               ]

apply :: (MonadError LispError m, MonadIO m) => LispVal -> [LispVal] -> m LispVal
apply (PrimitiveFunc func) args = liftEither $ func args
apply (IOFunc func) args = liftPrim $ func args
apply Func{..} args
  | isNothing _funcVararg && paramCount /= length args =
      throwError $ NumArgs (toInteger paramCount) args
  | otherwise = do
      env' <- bindVars _funcClosure (zip _funcParams args)
      env'' <- maybe (return env') (bindVarargs env') _funcVararg
      last <$> mapM (eval env'') _funcBody
  where paramCount = length _funcParams
        varargs = drop (length _funcParams) args
        bindVarargs env vararg = bindVars env [(vararg, List varargs)]
apply f _ = throwError $ NotFunction "Expected a function" f

applyProc :: (MonadError LispError m, MonadIO m) => [LispVal] -> m LispVal
applyProc [func, List args] = apply func args
applyProc (func:args) = apply func args
applyProc args = throwError $ NumArgs 1 args -- TODO: better error here (>0 expected?)

makePort :: (MonadError LispError m, MonadIO m) => IOMode -> [LispVal] -> m LispVal
makePort mode = oneArg f
  where f (String filename) = Port <$> liftIO (openFile filename mode)
        f arg = throwError $ TypeMismatch "string" arg

closePort :: (MonadError LispError m, MonadIO m) => [LispVal] -> m LispVal
closePort = oneArg f
  where f (Port port) = liftIO $ hClose port >> return (Bool True)
        f arg = throwError $ TypeMismatch "port" arg

readProc :: (MonadError LispError m, MonadIO m) => [LispVal] -> m LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = readExpr =<< liftIO (hGetLine port)
readProc [arg] = throwError $ TypeMismatch "port" arg
readProc args = throwError $ NumArgs 1 args -- TODO: better NumArgs error

writeProc :: (MonadError LispError m, MonadIO m) => [LispVal] -> m LispVal
writeProc [obj] = writeProc_ obj stdout
writeProc [obj, Port port] = writeProc_ obj port
writeProc [_, arg] = throwError $ TypeMismatch "port" arg
writeProc args = throwError $ NumArgs 2 args -- TODO

writeProc_ :: (MonadError LispError m, MonadIO m) => LispVal -> Handle -> m LispVal
writeProc_ obj port = liftIO $ hPrint port obj >> return (Bool True)

readContents :: (MonadError LispError m, MonadIO m) => [LispVal] -> m LispVal
readContents = oneArg f
  where f (String filename) = liftIO $ String <$> readFile filename
        f arg = throwError $ TypeMismatch "string" arg

load :: (MonadError LispError m, MonadIO m) => String -> m [LispVal]
load filename = readExprs =<< liftIO (readFile filename)

readAll :: (MonadError LispError m, MonadIO m) => [LispVal] -> m LispVal
readAll = oneArg f
   where f (String filename) = List <$> load filename
         f arg = throwError $ TypeMismatch "string" arg

primitiveBindings :: (MonadIO m) => m Env
primitiveBindings =
  bindPrims ioPrimitives IOFunc =<< bindPrims primitives PrimitiveFunc =<< nullEnv
  where bindPrims prims f = flip bindVars (prims & each._2 %~ f)
