{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Eval where

import Control.Lens ((%~), each, _2, (&))
import Control.Monad.Except
import Data.Either.Combinators
import Data.Maybe

import Types
import Env

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
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "unrecognized special form" badForm

apply :: (MonadError LispError m, MonadIO m) => LispVal -> [LispVal] -> m LispVal
apply (PrimitiveFunc func) args = liftEither $ func args
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

primitiveBindings :: (MonadIO m) => m Env
primitiveBindings =
  flip bindVars (primitives & each._2 %~ PrimitiveFunc) =<< nullEnv

primitives :: (MonadError LispError m) => [(String, [LispVal] -> m LispVal)]
primitives = [ ("+", nnn (+))
             , ("-", nnn (-))
             , ("*", nnn (*))
             , ("/", nnn div)
             , ("mod", nnn mod)
             , ("quotient", nnn quot)
             , ("remainder", nnn rem)
             , ("=", nnb (==))
             , ("<", nnb (<))
             , (">", nnb (>))
             , ("/=", nnb (/=))
             , ("<=", nnb (<=))
             , (">=", nnb (>=))
             , ("&&", bbb (&&))
             , ("||", bbb (||))
             , ("string=?", ssb (==))
             , ("string<?", ssb (<))
             , ("string>?", ssb (>))
             , ("string<=?", ssb (<=))
             , ("string>=?", ssb (>=))
             , ("symbol?", typeTest isSymbol)
             , ("string?", typeTest isString)
             , ("number?", typeTest isNumber)
             , ("car", oneArg car)
             , ("cdr", oneArg cdr)
             , ("cons", twoArg cons)
             , ("eq?", llb eqv)
             , ("eqv?", llb eqv)
             , ("equal?", llb equal)
             ]
  where typeTest f = oneArg $ return . Bool . f
        nnn = twoArg . wrapBinop extractNum Number
        nnb = twoArg . wrapBinop extractNum Bool
        bbb = twoArg . wrapBinop extractBool Bool
        ssb = twoArg . wrapBinop extractString Bool
        llb f = fmap Bool . twoArg f

oneArg :: (MonadError LispError m) => (LispVal -> m a) -> [LispVal] -> m a
oneArg f [x] = f x
oneArg _ xs = throwError $ NumArgs 1 xs

twoArg :: (MonadError LispError m) => (LispVal -> LispVal -> m a) -> [LispVal] -> m a
twoArg f [x, y] = f x y
twoArg _ xs = throwError $ NumArgs 2 xs

car :: (MonadError LispError m) => LispVal -> m LispVal
car (List (x:_)) = return x
car (DottedList (x:_) _) = return x
car arg = throwError $ TypeMismatch "pair" arg

cdr :: (MonadError LispError m) => LispVal -> m LispVal
cdr (List (_:xs)) = return $ List xs
cdr (DottedList [_] x) = return x
cdr (DottedList (_:xs) x) = return $ DottedList xs x
cdr arg = throwError $ TypeMismatch "pair" arg

cons :: (MonadError LispError m) => LispVal -> LispVal -> m LispVal
cons x (List xs) = return $ List (x:xs)
cons x (DottedList xs x1) = return $ DottedList (x:xs) x1
cons x y = return $ DottedList [x] y

eqv :: (MonadError LispError m) => LispVal -> LispVal -> m Bool
eqv = fix eqv_

eqv_ :: (MonadError LispError m) => (LispVal -> LispVal -> m Bool) -> LispVal -> LispVal -> m Bool
eqv_ _ (Bool x) (Bool y) = return $ x == y
eqv_ _ (Number x) (Number y) = return $ x == y
eqv_ _ (String x) (String y) = return $ x == y
eqv_ _ (Atom x) (Atom y) = return $ x == y
eqv_ eq (List xs) (List ys) =
  (length xs == length ys &&) <$> fmap and (zipWithM eq xs ys)
eqv_ eq (DottedList xs x) (DottedList ys y) =
  (&&) <$> eqv_ eq (List xs) (List ys) <*> eq x y
eqv_ _ _ _ = return False

primEqual_ :: [Unpacker (Except LispError)] -> LispVal -> LispVal -> Bool
primEqual_ unpackers x y = any (unpackEquals x y) unpackers

prefixEq :: (Applicative m)
         => (LispVal -> LispVal -> Bool)
         -> (LispVal -> LispVal -> m Bool)
         -> LispVal
         -> LispVal
         -> m Bool
prefixEq firstEq thenEq x y = (firstEq x y ||) <$> thenEq x y

equal :: (MonadError LispError m) => LispVal -> LispVal -> m Bool
equal = fix $ prefixEq (primEqual_ laxUnpackers) . eqv_

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _ = False

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _ = False

wrapBinop :: (Applicative m) => (a -> m b) -> (c -> d) -> (b -> b -> c) -> a -> a -> m d
wrapBinop unwrap wrap f x y = fmap wrap $ f <$> unwrap x <*> unwrap y

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
