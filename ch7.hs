import Data.Char
-- 1.
applyFromWhere :: (a -> b) -> [a] -> (a -> Bool) -> [b]
applyFromWhere f xs p = map f (filter p xs)

-- 2.
-- a.
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (&&) True . map p 

-- b.
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (||) False . map p

-- c.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x       = x:(takeWhile' p xs)
                    | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p = foldr (\x xs -> if p x then (x:xs) else []) [] 

-- d.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs

dropWhile'' :: (a -> Bool) -> [a] -> [a]
dropWhile'' p = foldr (\x xs -> if p x then xs else (x:xs)) []

-- 3.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x):xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then (x:xs) else xs) []

-- 4.
dec2int :: [Int] -> Int
dec2int = foldl (\x xs -> (x * 10) + xs) 0

-- 5.
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f a b = f (a,b)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f (a,b) = f a b

-- 6.
unfold' :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold' p h t x | p x = []
                | otherwise = h x : unfold' p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold' (== 0) (`mod` 2) (`div` 2)

chop :: Int -> [Bit] -> [[Bit]]
chop n = unfold' null (take n) (drop n)

chop8 :: [Bit] -> [[Bit]]
chop8 = chop 8

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold' null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold' (\x -> False) f f

-- 7.
bin2int :: [Bit] -> Int
bin2int = foldr (\x xs -> x + 2 * xs) 0

make8 :: [Bit] -> [Bit]
make8 = take 8 . (++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- now we define a new encoding function that 
-- includes a parity bit at the end of every
-- 8 bit word. This bit is discarded if it 
-- matches with the parity of the 1's in the preceding
-- 8 bits, otherwise an error is detected and the 
-- transmission aborts. We simulate a faulty 
-- channel that forgets the first bit using the tail function

makeParity :: [Bit] -> Bit
makeParity = foldr (\x y -> if x == 1 then 1 - y else y) 0

encodeWithParity :: String -> [Bit]
encodeWithParity = concat . map ((\x -> (make8 x) ++ [makeParity x]) . int2bin . ord)

checkParity :: [Bit] -> Bool
checkParity x = (makeParity (take 8 x)) == (last x)

chop9 :: [Bit] -> [[Bit]]
chop9 = chop 9

decodeWithParity :: [Bit] -> String
decodeWithParity = map (chr . bin2int . take 8 . (\x -> if checkParity x then x else error "Transmission Error Detected")) . chop9

safeTransmit :: String -> String
safeTransmit = decodeWithParity . channel . encodeWithParity

faultyChannel :: [Bit] -> [Bit]
faultyChannel = concat . map ((:) 1 . tail ). chop9

experimentalTransmit :: String -> String
experimentalTransmit = decodeWithParity . faultyChannel . encodeWithParity

-- 9.
altMapV :: (a -> b) -> (a -> b) -> Bool -> [a] -> [b]
altMapV _ _ _ [] = []
altMapV f g v (x:xs) | v         = f x : altMapV f g False xs
                     | otherwise = g x : altMapV f g True  xs

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap = (\f g xs -> altMapV f g True xs)

-- 10.
luhn :: [Int] -> Bool
luhn = (==) 0 . (`mod` 10) . sum . altMap id (\x -> if x * 2 > 9 then x * 2 - 9 else x * 2) . reverse 