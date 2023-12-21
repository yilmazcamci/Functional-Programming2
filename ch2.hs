-- 2.
 n = a `div` length xs
     where 
         a = 10
         xs = [1,2,3,4,5]

-- problems: 
-- `div must be between backward quotes
-- n must be lowercase, as with all names of functions (and variables, although variables are constant functions)
-- xs is a definition at the same level as a, hence should start in exactly the same column

-- 3.
-- (2^3)*4
-- (2*3)+(4*5)
-- 2+(3*(4^5))

-- 4.
last = head . reverse
last x = x !! (length x - 1)

-- 5.
init = reverse . drop 1 . reverse
init = reverse . tail . reverse
