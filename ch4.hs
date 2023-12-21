-- 1.
halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs) 
    where half = length xs `div` 2

-- 2.
third :: [a] -> a

-- using head and tail:
third = head . tail . tail

-- using indexing:
third = (!!2)

-- using pattern matching:
third (x:y:z:ss) = z

-- 3.
safetail :: [a] -> [a]

-- using pattern matching:
safetail [] = []
safetail (x:xs) = xs

-- using guarded equations:
safetail xs | null xs = xs
            | otherwise = tail xs

-- using conditional expressions:
safetail xs = if null xs then xs else tail xs


-- 4.
(||) :: Bool -> Bool -> Bool

-- explicit:
True  || False = True
True  || True  = True
False || True  = True
False || False = False

-- wildcard pattern:
False || False = False
_     || _     = True

-- first argument:
False || b     = b
True  || _     = True


-- matching against b twice:
b     || b     = b
_     || _     = True


-- guarded:
b || c | b == c    = b
       | otherwise = True

-- 5.
(&&) :: Bool -> Bool -> Bool

-- working definition:
-- True && True = True
-- _    && _    = False

a && b | a         = if b then True else False
       | otherwise = False

-- note: very ugly, you can trust Bools!
a && b | a = b
       | otherwise = False

-- 6. this is actually the above implementation


-- 7. 
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))

-- 8.
luhnDouble :: Int -> Int
luhnDouble x = if 2*x > 9 then 2*x - 10 else 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (lD w + lD x + lD y + lD z) `mod` 10 == 0
    where lD = luhnDouble

