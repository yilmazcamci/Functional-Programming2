module Char where

import Data.Char

(~~) :: String -> String -> Bool
(~~) str1 str2 = map toLower str1 == map toLower str2

reverseCase :: String -> String
reverseCase str = map reverseCaseChar str 
    where
        reverseCaseChar ch | isUpper ch = toLower ch
                           | isLower ch = toUpper ch
                           | otherwise = ch


shift :: Int -> Char -> Char
shift n ch | isUpper ch = chr(ord 'A' + ((ord ch - ord 'A' + n) `mod` 26))
           | otherwise = ch


caesar :: Int -> String -> String
caesar n = (map (shift n)) . (map toUpper)

msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"

bruteforce :: [(Int, String)]
bruteforce = zip [1..26] (map (\n -> caesar n msg) [1..26])

-- and we quickly read that for 5, we decode the message with:
-- "FIRST I MUST SPRINKLE YOU WITH FAIRY DUST"
-- not very sophisticated, but here "brute force" means iterating through 26 possible keys

