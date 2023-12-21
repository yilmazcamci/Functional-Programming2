-- 1.
-- [`a`,`b`,`c`] :: [Char]
-- (`a`,`b`,`c`) :: (Char, Char, Char)
-- [(False,`O`),(True,`1`)] :: [(Bool, Char)]
-- ([False,True],[`0`,`1`]) :: ([Bool],[Char])
-- [tail,init,reverse] :: [[a] -> [a]]

-- 2.
bools = [True]
nums = [[12],[2,3,4],[]]
add x y z = z + y + z
copy = \x -> (x,x)
apply f x = f x

-- 3.
second :: [a] -> a
second = head . tail

swap :: (a,b) -> (b,a)
swap = \(x,y) -> (y,x)

pair :: a -> b -> (a,b)
pair = \x y -> (x,y)

double :: (Num a) => a -> a
double = \x -> x*2

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f = f . f

-- 5.
-- Two functions are equal only if they return equal results
-- for all possible arguments in their domain. However, these
-- domains can be very large, e.g. the function (+1) has as its
-- domain all the integers. It is infeasible to check whether
-- two functions return the same results for all possible 
-- arguments in the domain when a domain is so large.
-- That is why in general it is not feasible for function
-- types to be instances of the Eq class.
--
-- It is feasible, however, when the domain of the functions
-- is a sufficiently small set. For example, functions that 
-- have n Bool type arguments as their arguments have a
-- domain of only cardinality 2^n, hence for n sufficiently
-- small checking equality of outputs is feasible
