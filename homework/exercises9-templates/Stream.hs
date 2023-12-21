module Stream where

import qualified Data.List as List
import Prelude hiding (head, tail, repeat, map, zipWith, filter, take, drop, concat, cycle, sum)

data Stream a = a :> Stream a
infixr 5 :>

instance (Show a) => Show (Stream a) where
  show s = "(" List.++ showN (16::Int) s List.++ ")"
    where
    showN 0 _         = "..."
    showN n (x :> xs) = show x List.++ " :> " List.++ showN (n-1) xs

from :: Integer -> Stream Integer
from n = n :> from (n + 1)

head :: Stream a -> a
head (x :> xs) = x

tail :: Stream a -> Stream a
tail (x :> xs) = xs

repeat :: a -> Stream a
repeat x = x :> repeat x

map :: (a -> b) -> (Stream a -> Stream b)
map f (x:> xs) = f x :> map f xs

zipWith :: (a -> b -> c) -> (Stream a -> Stream b -> Stream c)
zipWith f (xa:>xas) (xb:>xbs) = f xa xb :> zipWith f xas xbs

filter :: (a -> Bool) -> Stream a -> Stream a
filter p (x:>xs) = if p x then x :> filter p xs else filter p xs

-- Explain what happens if we call filter (\x -> False) (from 0).

-- Answer: using the principles of lazy evaluation, i.e. 
-- * evaluate outermost expression first, unless this is a lambda expression;
-- * use pointers to duplicated common subexpressions;
-- , we find:
-- filter (\x -> False) (from 0) 
-- = filter (\x -> False) (0:> from 1)
-- = filter (\x -> False) (from 1)
-- = filter (\x -> False) (1 :> from 2)
-- = filter 
-- There will be a repetition of two alternating steps, 
-- * one where Haskell will evaluate from n = n :> from (n+1), expanding the from-stream by 1 element,
-- * the next step evaluating the outermost expression by evaluation of \x -> False = False, thereby removing n from the stream. 
-- But this will never terminate.

toList :: Stream a -> [a]
toList (x:>xs) = x: toList xs

cycle :: [a] -> Stream a
cycle l = cycleCat l l where
  cycleCat [] ys = cycleCat ys ys
  cycleCat (x:xs) ys = x:>cycleCat xs ys

nat, fib :: Stream Integer
nat = 0 :> zipWith (+) nat (repeat 1)
fib = 0 :> 1 :> zipWith (+) fib (tail fib)

primetwins :: Stream (Integer,Integer)
primetwins = filter (\tp -> fst tp + 2 == snd tp) (zipWith (\x y -> (x,y)) primes (tail primes))

primes :: Stream Integer
primes = sieve (from 2) where
  sieve (p:>xs) = p :> sieve (filter (\x -> x `mod` p /= 0) xs)

-- To guarantee that every element of either input stream appears in the output
-- stream at a (sufficiently large) position, we can alternate the elements
-- from the first stream with elements from the second stream. Any element 
-- from the second stream at pos. n will be encountered at pos 2n-1 in combine,
-- and any element from the first stream at pos. n will be encountered at pos. 2n.
combine :: Stream a -> Stream a -> Stream a
combine (x:>xs) (y:>ys) = x:>y:>combine xs ys
-- note that due to the statement infixr 5, :> will associate to the right, so 
-- we omit the parentheses aroubt y:>comnine xs ys.



diag :: Stream (Stream a) -> Stream a
