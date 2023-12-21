module Unfold where

import Data.List (unfoldr)
import Prelude hiding (take,zip,(++))

-- define all the below funtions using `unfoldr`
bits :: Int -> [Int]
bits = unfoldr (\d -> if d == 0 then Nothing else Just (d `mod` 2, d `div` 2))

--zip :: [a] -> [b] -> [(a,b)]
zip l1 l2 = unfoldr grow (l1, l2)  where
  grow (ls1, ls2) = case (ls1, ls2) of
    ([], _) -> Nothing
    (_, []) -> Nothing
    (x:xs, y:ys) -> Just ((x,y), (xs,ys))

take :: Int -> [a] -> [a]
take n ls = unfoldr grow (n,ls) where
  grow (_,[]) = Nothing
  grow (0,_) = Nothing
  grow (k, x:xs) = Just (x, (k-1, xs))

primes :: [Integer]
primes = unfoldr (\lst -> if lst == [] then Nothing else Just (head lst, [ n | n <- lst, n `mod` (head lst) /= 0 ])) [2..]

-- alternative implementation of `primes`:
primes' = sieve [2..]
  where sieve (p:xs) = p : sieve [ n | n <- xs, n `mod` p /= 0 ]

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

(++) :: [a] -> [a] -> [a]
(++) = flip plusplus where
  plusplus l2 = apo f where
    f l1 = case l1 of
      []     -> Left l2
      (x:xs) -> Right (x,xs)

insert :: (Ord a) => a -> [a] -> [a]
insert x = apo f where
  f lst = case lst of
    [] -> Left []
    (y:ys) | x < y      -> Left (x : y : ys)
           | otherwise  -> Right (y,ys)

unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
unfoldrApo f = apo g where
  g seed = case f seed of
    Nothing -> Left []
    Just (x,b) -> Right (x,b)