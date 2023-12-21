-- 1.
-- double (double 2) = double 2 + double 2 = (2 + 2) + (2 + 2) = 4 + 4 = 8
--
-- 2.
-- definition of sum:
--
-- sum :: sum :: (Foldable t, Num a) => t a -> a
-- sum []     = 0
-- sum (x:xs) = x + sum xs
--
-- from this it follows that for all Num x
-- sum [x] = x + sum [] = x + 0 = x
--
-- 3. 
product :: (Num a) => [a] -> a
product [] = 1
product (x:xs) = x * product xs

-- by this definition, 
-- product [2,3,4] = 2 * product [3,4]
--                 = 2 * (3 * product [4]) 
--                 = 2 * (3 * ( 4 * product [1])) 
--                 = 2 * (3 * (4 * 1)) = 24

-- 4. 
qsortrev :: (Ord a) => [a] -> [a]
qsortrev [] = []
qsortrev (x:xs) = qsortrev . filter (=>x) xs ++ [x] ++ qsortrev . filter (<x) xs

-- 5.
-- replacing <= by < would produce a sorting algorithm 
-- that removes duplicates from the list while sorting.
-- this is because for every pivot (and all elements of
-- the list will be taken as a pivot once during the
-- execution of qsort) we only put into any part of the
-- sorted list items that are strictly less or
-- greater than the pivot, so duplicates of the pivot
-- are filtered out as a side-effect.
--
-- as an example, consider 
--
-- qsort [2,2,3,1,1] = qsort [1,1] ++ [2] ++ qsort [3]
--                   = (qsort [] ++ [1] ++ qsort []) ++ [2] ++ (qsort [] ++ [3] ++ qsort [])
--                   = ([] ++ [1] ++ []) ++ [2] ++ ([] ++ [3] ++ [])
--                   = [1] ++ [2] ++ [3]
--                   = [1,2,3]

-- 
