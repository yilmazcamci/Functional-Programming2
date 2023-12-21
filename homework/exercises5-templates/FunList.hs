module FunList where

--define using the _list design pattern_
compose :: [a -> a] -> (a -> a)
compose [] = id
compose (x:xs) = x . (compose xs)



--define using `foldr`
compose' :: [a -> a] -> (a -> a)
compose' = foldr (.) id 


--Explain _what_ the following function computes, and _how_ it computes it
{-

 Your explanation here

foo n = n!
since
foo n = compose (map (*) [1..n]) 1 = 
    (map (1*) . (map (2* . (... (map ((n-1)*) . (map (n*) . id)))...))) 1 = 
        map (\x -> 1*2*...*n*x) 1 = 
            1*2*...*n = n!
-}
foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1

--define in terms of *only* `map` and `compose`
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' st ba lst = compose (map st lst) ba
