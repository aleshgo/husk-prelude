{-# LANGUAGE DeriveFunctor #-}

module Data.Result where

import HuskPrelude

data Result a
  = Err ByteString
  | Ok a
  deriving (Eq, Show, Functor)

instance Applicative Result where
  pure = Ok
  Ok f <*> a = fmap f a
  Err e <*> _ = Err e

instance Monad Result where
  Ok a >>= f = f a
  Err e >>= _ = Err e

result :: (ByteString -> b) -> (a -> b) -> Result a -> b
result f _ (Err e) = f e
result _ g (Ok a) = g a

isErr :: Result a -> Bool
isErr (Err _) = True
isErr (Ok _) = False

isOk :: Result a -> Bool
isOk (Err _) = False
isOk (Ok _) = True

leftToErr :: ByteString -> a -> Result b
leftToErr = const . Err
