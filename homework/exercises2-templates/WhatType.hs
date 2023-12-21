module WhatType where

f0 :: (Char, Char) -> Bool
f0 (x,y)   = x == 'F' && y == 'P'

f1 :: String -> String
f1 s       = s ++ ", cruel world!"

f2 :: a -> (b,c) -> (c,a,b)
f2 x (y,z) = (z,x,y)

f3 :: Char -> Char
f3 ' '     = '_'
f3 c       = c

f4 :: String -> String -> String
f4 x y
  | x == ""   = y
  | otherwise = x

--f5 :: Bool -> a -> a -> (a,a)
f5 b x y   = if b then (x,y) else (y,x)

f6 :: a -> b -> a
f6 x       = \y -> x

f7 :: String -> String
f7         = ("Haskell" ++)
