module Lego where

import Data.List
import Data.Tuple
import Data.Function

removeAt :: Int -> [a] -> [a]
removeAt n xs = if n > length xs then xs else take (n-1) xs ++ drop n xs

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos xs = sort (zip xs [0..])


sortedPos :: (Ord a) => [a] -> [(a,Int)]
sortedPos xs = map (\((x,y),z) -> (x,z)) (sortBy (compare `on` (snd . fst)) (zip (sortWithPos xs) [0..]))