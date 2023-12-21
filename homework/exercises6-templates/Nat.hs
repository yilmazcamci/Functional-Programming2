module Nat where

data Nat = O | S Nat
  deriving (Show)

--fromNat :: (Num t) => Nat -> t
--fromNat O     = ...
--fromNat (S x) = ...

--toNat :: (Ord t, Num t) => t -> Nat

--instance Eq Nat where 
--  ...

--instance Ord Nat where 
--  ...

instance Enum Nat where
  succ x     = error "TODO"
  pred x     = error "TODO"
  toEnum x   = error "TODO"
  fromEnum x = error "TODO"
