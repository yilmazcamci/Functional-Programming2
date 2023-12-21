module HOF where

import Prelude hiding (const)

{- exercise 5.1 -}

const :: p1 -> p2 -> p1
const x _y   = x

($->) :: t1 -> (t1 -> t2) -> t2
x $-> y      = y x


oper :: Fractional p => String -> p -> p -> p
oper "mul" n = (*n)
oper "div" n = (n/)
oper _     _ = error "not implemented"

mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap f xs  = map (map f) xs

without p :: (a -> Bool) -> [a] -> [a]
without p    = filter (not . p)

on :: (a -> a -> c) -> (d -> a) -> d -> d -> c
on f g x y   = f (g x) (g y)

{- exercise 5.2 -}

f1 :: (Num a) => a -> a
f1 = (* 5) . (+ 1)
-- f1 x = x * 5 + 5

f2 :: (Num a) => a -> a
f2 = (+ 1) . (* 5)
-- f2 x = x * 5 + 1

f3 :: (Num a, Ord a) => a -> a
f3 = (min 100) . (max 0)
-- f3 x = min (100, max(x 0)) = x if x < 0 else 0

f4 :: [a] -> Bool
f4 = (<5) . length
-- f4 lst = True if lst less than 5 elements

