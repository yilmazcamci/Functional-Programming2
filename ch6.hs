-- 1.
-- Applying factorial to a negative integer will result in a stack overflow.
-- This is because this call will never reach a base case (the call to factorial
-- n for n < 0 will call factorial n - 1 which is also < 0 and this will always
-- stay negative thus never reaching the case 0.
-- To prevent this, we can give the following definition:

factorial :: Int -> Int
factorial 0 = 1
factorial n | n > 0 = n * (factorial (n - 1)) -- throws exception if n < 0

-- 2.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + (sumdown (n - 1))

-- 3.
pow :: Int -> Int -> Int
pow n 0 = 1
pow n k | k > 0 = n * pow n (k - 1)
--   pow 2 3  
-- = 2 * pow 2 2 
-- = 2 * (2 * pow 2 1)
-- = 2 * (2 * (2 * pow 2 0))
-- = 2 * (2 * (2 * 1))
-- = 8

-- 4. 
euclid :: Int -> Int -> Int
euclid 0 k = k
euclid n 0 = n
euclid n k | k >= n = euclid n (k - n)
           | k <  n = euclid (n - k) k

-- 5.
--   length [1,2,3] 
-- = 1 + length [2,3]
-- = 1 + (1 + length [3])
-- = 1 + (1 + (1 + length []))
-- = 1 + (1 + (1 + 0))
-- = 3

--   drop 3 [1,2,3,4,5]
-- = drop 2 [2,3,4,5]
-- = drop 1 [3,4,5]
-- = drop 0 [4,5]
-- = [4,5]

--   init [1,2,3]
-- = 1 : init [2,3]
-- = 1 : (2 : init [3])
-- = 1 : (2 : [])
-- = [1,2]

-- 6.
-- a.

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && (and' xs)

-- b.
concat' :: [[a]] -> [a]
concat' []     = []
concat' (b:bs) = b ++ (concat' bs)

-- c.
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : (replicate (n - 1) x)

-- d.
at' :: [a] -> Int -> a
at' (x:xs) 1 = x
at' (x:xs) n  = at' xs (n - 1)

-- e.
elem' :: Eq a => a -> [a] -> Bool
elem' x []                 = False
elem' x (y:ys) | x == y    = True
               | otherwise = elem' x ys

-- 7.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : (merge xs (y:ys))
                    | otherwise = y : (merge (x:xs) ys)

-- 8.
halve :: [a] -> ([a],[a])
halve xs = (take h xs, drop h xs) where h = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst h)) (msort (snd h)) where h = halve xs

-- 9.
sum' ::  Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + (sum' xs)

take' ::  Int -> [a] -> [a]
take' 0 _      = []
take' k []     = []
take' k (x:xs) = x:(take' (k-1) xs)

last' :: [a] -> a
last' [x]    = x
last' (x:xs) = last' xs
