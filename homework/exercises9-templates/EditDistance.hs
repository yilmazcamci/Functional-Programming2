module EditDistance where

import Data.Array

naiveEditDistance :: String -> String -> Int
naiveEditDistance xs ys = distance xs ys
  where
  distance :: String -> String -> Int
  distance [] ys = length ys
  distance xs [] = length xs
  distance (x:xs) (y:ys) = minimum [1+distance xs (y:ys), 1+distance (x:xs) ys, cost x y+distance xs ys]

  cost x y = if x==y then 0 else 1


editDistance :: String -> String -> Int
editDistance xs ys = helper(0, 0)
 where
 helper :: (Int,Int) -> Int
 helper (a, b)
  | a == max_x = max_y - b
  | b == max_y = max_x - a
  | xs!!a == ys!!b = distArray ! (a+1,b+1)
  | otherwise = 1 + minimum [distArray ! (a + 1, b), distArray ! (a, b+1), distArray ! (a+1, b+1) ]
 distArray :: Array (Int,Int) Int
 distArray = array ((0,0), (max_x, max_y)) [(ij, helper ij) | i <- [0..max_x], j <- [0..max_y], let ij = (i,j)]
 
 max_x = length xs
 max_y = length ys
