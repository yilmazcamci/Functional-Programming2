module PolyType where

f8 :: (Ord a) => a -> a -> a
f8 x y  = if x <= y then x else y

f9 :: Bool -> Bool -> Bool
f9 x y  = not x || y

f10 :: (Eq a, Num a) => a -> a -> a
f10 x y
  | x == 0    = y
  | otherwise = x + y

f11 :: a -> a -> a
f11 x y = get 0
  where get n = if n == 0 then x else y

-- 1. Which of these functions can be used on arguments of type String?

-- Answer:
-- f8, f11.
-- Explanation: String is in the type class Eq so it can be passed to f8, 
-- but not to f9, which works on Bool,
-- and not to f10, because String is not in the class Num,
-- but any type can be passed to f11.

-- 2. For each function, determine if it is parametric polymorphic, ad-hoc polymorphic (also known
-- as overloaded), or not polymorphic.

-- Answer:
-- f8 is parametric polymorphic: it is polymorphic with a uniform implementation for all 
-- instances of the type it is polymorphic over.

-- f9 is not polymorphic

-- f10 is parametric polymorphic: it has a uniform implementations for all instances of 
-- the type a it is polymorphic over.

-- f11 is parametric polymorphic for the same reason: we again have only one implemenation
-- for all instances of a.

-- If a function is not polymorphic, state what t is. Otherwise, determine the type classes (if
-- any) that the type used as t must be an instance of.

-- Answer:
-- f8 : t is instance of Ord

-- f9 : t is Bool

-- f10 : t is instance of Eq and Num

-- f11 : t is any type
