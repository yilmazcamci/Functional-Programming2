module ListComprehensions where

-- g0 :: [a] -> [b] -> [(a,b)]
-- polymorphic, no type class constraints
g0 as bs = [ (a,b) | a <- as, b <- bs ]

-- name: cartesian

-- computes the cartesian product of two lists, 
-- consisting of all possible pairs than can be 
-- formed where the first element comes from as 
-- and the second element comes from bs

-- g1 :: (Num a, Enum a) => a -> [b] -> [b]
-- polymorphic with the type contraint that n
-- be enumerable and numeric.
g1 n y   = [ y | i <- [1..n] ]

-- name: repeat
-- computes a list that contains y n times


-- g2 :: (Num a, Enum a, Ord a) => a -> [b] -> [(a,b)]
-- polymorphic with type constraint of a being enumerable
-- ordered and numeric.
g2 n xs  = [ x | (i,x) <- zip [0..] xs, i < n ]

-- name: add_counter
-- computes a list of pairs where the firs element is a 
-- counter i running from 0 to n-1 and the second element
-- is the i-th element of the list.
-- for example denumerate 3 ['a','b','c','d'] = [(0,'a'), (1,'b'), (2,'c')]

-- g3 :: (Eq b, Num r, Enum r) => b -> [b] -> [(r, b)]
-- polymorphic with type constraint on b being that it
-- is equible, and the result counter (Int or Integer) 
-- should be enumerable and numeric.
g3 a xs  = [ i | (i,x) <- zip [0..] xs, x == a]


-- list_occurrences_and_pos
-- lists occurrences of a in xs, paired with an index i to denote
-- the index at which the occurrence of a is found in xs


-- g4 :: [a] -> [a] -> [a]
-- polymorphic on a, no type constraints
g4 xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

-- name: alternating_merge
-- merges two list, alternatingly taking an element
-- x from xs and then an element y from ys
-- e.g. g4 [1..4] [5..8] = [1,5,2,6,3,7,4,8]

-- g5 :: [[a]] -> [a]
-- polymorphic on a, no type contraints on a
g5 xss   = [ x | xs <- xss, x <- xs ]

-- name: union
-- takes the unions of all the lists in the list xss,
-- computer science students would call it "flatten" or smth.
