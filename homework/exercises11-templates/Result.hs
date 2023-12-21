{-# LANGUAGE InstanceSigs #-}
module Result where

import Data.List

data Result a = Okay a | Error [String]
  deriving (Eq,Ord,Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f (Error l) = Error l 
  fmap f (Okay x) = Okay (f x)

instance Applicative Result where
  pure :: a -> Result a 
  pure = Okay 
  (<*>) :: Result (a -> b) -> Result a -> Result b
  (Error l1) <*> (Error l2) = Error $ l1 ++ l2
  (Error l1) <*> _          = Error l1
  _          <*> (Error l2) = Error l2
  (Okay f)   <*> (Okay x)   = Okay $ f x

instance Monad Result where
  return = pure
  (>>=) :: Result a -> (a -> Result b) -> Result b
  (Error l) >>= f = Error l
  (Okay x) >>= f = f x