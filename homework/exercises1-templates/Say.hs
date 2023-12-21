module Say where

say :: Integer -> String
say  0 = "zero"
say  1 = "one"
say  2 = "two"
say  3 = "three"
say  4 = "four"
say  5 = "five"
say  6 = "six"
say  7 = "seven"
say  8 = "eight"
say  9 = "nine"
say 10 = "ten"
say 11 = "eleven"
say 12 = "twelve"
say 13 = "thirteen"
say 15 = "fifteen"
say 18 = "eighteen"
say 20 = "twenty"

say 30 = "thirty"
say 40 = "forty"
say 50 = "fifty"
say 60 = "sixty"
say 70 = "seventy"
say 80 = "eighty"
say 90 = "ninety"


say n 
    | n <= 20   = say (n `mod` 10) ++ "teen"
    | n <= 99   = say ((n `div` 10) * 10) ++ " " ++ say (n `mod` 10)
    | n <= 999  = say (n `div` 100) ++ " hundred" ++ (if (n `mod` 100 == 0) then "" else " " ++ say (n `mod` 100))
    | otherwise = say (n `div` 1000) ++ " thousand" ++ (if (n `mod` 1000 == 0) then "" else " " ++ say (n `mod` 1000))

