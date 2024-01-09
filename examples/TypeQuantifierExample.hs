-- forall and extension ScopedTypeVariables


-- The following code does not compile: (1)
f :: [a] -> [a]
f xs = ys ++ ys
  where ys :: [a]
        ys = reverse xs
        
-- reason: the compiler cannot match the type of ys ++ ys :: [a],
-- with the return type f xs :: [a]. 
-- Why? This is because:
{-
    • Couldn't match type ‘a1’ with ‘a’
      Expected: [a1]
        Actual: [a]
      ‘a1’ is a rigid type variable bound by
        the type signature for:
          ys :: forall a1. [a1]
        at TypeQuantifierExample.hs:7:9-17
      ‘a’ is a rigid type variable bound by
        the type signature for:
          f :: forall a. [a] -> [a]
        at TypeQuantifierExample.hs:5:1-15
    • In the first argument of ‘reverse’, namely ‘xs’
      In the expression: reverse xs
      In an equation for ‘ys’: ys = reverse xs
    • Relevant bindings include
        ys :: [a1] (bound at TypeQuantifierExample.hs:8:9)
        xs :: [a] (bound at TypeQuantifierExample.hs:6:3)
        f :: [a] -> [a] (bound at TypeQuantifierExample.hs:6:1)
-}

-- We want this: (2)
f :: [a] -> [a]
f xs = ys ++ ys
  where ys = reverse xs
  
-- The above code compiles, since the type of ys can be inferred from the context
-- f. We impose upon Haskell that ys :: [a], where a is any type, in the declaration (1)

-- In other words, the a in ys :: [a] "hides" the a in f :: [a] -> [a],
-- or you could it say it is a different type variable. a stands for "any type",
-- not for "that type that is referred to with a in line 5"

-- To bind the a in line 7 (which is part of line 6) to the
-- a in line 5, we use use ScopedTypeVariables and write the following code: (3)
{-# LANGUAGE ScopedTypeVariables #-}

f :: forall a. [a] -> [a]
f xs = ys ++ ys
  where ys :: [a]
        ys = reverse xs
        
-- forall and extension RankNTypes:

