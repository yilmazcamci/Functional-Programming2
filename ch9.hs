-- 1.
import System.IO
import Data.List
import Data.Ord
-- decides whether the first list is chosen from the second.
-- running time O(n^2)

deleteIfPresent :: Eq a => a -> [a] -> (Bool, [a])
deleteIfPresent _ [] = (False, [])
deleteIfPresent x (y:ys) | x == y    = (True, ys)
                         | otherwise = let d = deleteIfPresent x ys in (fst d, y:(snd d))    
    

isChoice' :: (Show a, Eq a) => [a] -> [a] -> IO Bool
isChoice' [] _ = pure True
isChoice' _ [] = pure False
isChoice' (x:xs) ys = let (present, ys') = deleteIfPresent x ys in
                    if present then do 
        putStrLn (show ys')
        isChoice' xs ys' 
                    else pure False
         
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys = let (present, ys') = deleteIfPresent x ys in
    if present then isChoice xs ys' else False 
         
                 
-- Test cases

test1 = isChoice ([]::[Int]) [] == True

test2 = isChoice ([]::[Int]) [1, 2, 3] == True

test3 = isChoice ([1, 2, 3]::[Int]) [1, 2, 3, 4, 5] == True

test4 = isChoice ([1, 2, 3]::[Int]) [3, 2, 1, 4, 5] == True

test5 = isChoice ([1, 2, 3]::[Int]) [4, 5, 6] == False

test6 = isChoice ([1, 2, 2, 3]::[Int]) [3, 2, 1, 4, 5] == False

-- A more efficient implementation that runs in O(n log n) time
-- if the type a has an ordering on it:
-- first sort the lists in ascending order

type Comparator a = a -> a -> Ordering

isChoiceOrd :: Comparator a -> [a] -> [a] -> Bool
isChoiceOrd comp xs ys = isSubSequence comp (sortBy comp xs) (sortBy comp ys)

-- Where IsSubSequence is a greedy O(n) algorithm that 
-- compares the first elements, deletes them if they match,
-- else terminates if the element of the first list is
isSubSequence :: Comparator a -> [a] -> [a] -> Bool
isSubSequence comp [] _ = True
isSubSequence comp _ [] = False
isSubSequence comp (x:xs) (y:ys) = case comp x y of
    EQ -> isSubSequence comp xs ys
    GT -> isSubSequence comp (x:xs) ys
    LT -> False
    
isChoice'' :: Ord a => [a] -> [a] -> Bool
isChoice'' = isChoiceOrd compare

-- Test cases

test1' = isChoice'' ([]::[Int]) [] == True

test2' = isChoice'' ([]::[Int]) [1, 2, 3] == True

test3' = isChoice'' ([1, 2, 3]::[Int]) [1, 2, 3, 4, 5] == True

test4' = isChoice'' ([1, 2, 3]::[Int]) [3, 2, 1, 4, 5] == True

test5' = isChoice'' ([1, 2, 3]::[Int]) [4, 5, 6] == False

test6' = isChoice'' ([1, 2, 2, 3]::[Int]) [3, 2, 1, 4, 5] == False

-- 3. 


