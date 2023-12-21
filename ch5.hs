import Data.Char

-- 1.
s :: Int
s = sum [x^2 | x <- [1..100]]

-- 2.
grid :: Int -> Int -> [(Int,Int)]
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]

-- 3.
square :: Int -> [(Int,Int)]
square n = [ (x,y) | (x,y) <- grid n n, x /= y]

-- 4.
-- replicate :: Int  -> a -> [a]
-- replicate n v = [v | i <- [1..n]]

-- 5.
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6. 
factors :: Int -> [Int]
factors x = [z | z <- [1..(x-1)], x `mod` z == 0]

perfects :: Int -> [Int]
perfects l = [x | x <- [1..l], x == sum (factors x)]

-- 7.
altcompr :: [(Int,Int)]
altcompr = concat [[(1,y) | y <- [3,4]],[(2,y) | y <- [3,4]]]

-- 8.
find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions w ws = find w (zip ws [0..])

-- 9.
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- 10.
-- Caesar cipher:
let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | isUpper c = ord c - ord 'A'

int2let :: Bool -> Int -> Char
int2let u n | u         = chr (ord 'A' + n)
            | otherwise = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let (isUpper c) ((let2int c + n) `mod` 26)
          | isUpper c = int2let (isUpper c) ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

decode :: Int -> String -> String
decode n = encode ((-n) `mod` 26)
