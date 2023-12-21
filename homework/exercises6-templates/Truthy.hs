module Truthy where

class Truthy a where
    truthy :: a -> Bool

instance Truthy Bool where
    truthy = id

instance Truthy Integer where
    truthy = (== 0)

infixr 3 &&&
infixr 2 |||

(&&&) :: Truthy a => a -> a -> Bool
x &&& y = truthy x && truthy y

(|||) :: Truthy a => a -> a -> Bool
x ||| y = truthy x || truthy y


ifThenElse :: Truthy a => a -> b -> b -> b 
ifThenElse x y z = if truthy x then y else z

