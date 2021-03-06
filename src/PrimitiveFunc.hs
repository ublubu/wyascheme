{-# LANGUAGE FlexibleContexts #-}

module PrimitiveFunc where

import Control.Monad.Except

import Types
import Unpack

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

