module DigitalSorting where

import Data.List
import Data.Char
import Data.Ord
import Data.Maybe

class Rankable key where
  rank :: [(key,a)] -> [[a]]

digitalSortOn :: (Rankable key) => (v -> key) -> [v] -> [v]
digitalSortOn f = concat . rank . map (\x->(f x, x))

digitalSort :: (Rankable key) => [key] -> [key]
digitalSort = digitalSortOn id

genericRank :: (Ord key) => [(key,a)] -> [[a]]
genericRank = map (map snd) . groupBy (\x y -> fst x == fst y) . sortBy (comparing fst) 

instance Rankable Int where
  rank = genericRank

instance Rankable Integer where 
  rank = genericRank

instance Rankable Char where
  rank = genericRank

instance Rankable Bool where
  rank = map reverse . rankIntermediate [[],[]] where
    rankIntermediate [l1, l2] [] = [l1, l2]
    rankIntermediate [l1, l2] (x:xs) 
      | fst x = rankIntermediate [l1, (snd x):l2] xs
      | otherwise = rankIntermediate [(snd x):l1, l2] xs

-- Comment: we could have used ++ in intermediate steps, but that is O(n^2) while an extra reverse is O(n)

instance (Rankable key1, Rankable key2) => Rankable (key1,key2) where
  rank = union . map rank . rank . map assoc where
    union lst = [x | xs <- lst, x <- xs]
    assoc ((a1, a2), a3) = (a1, (a2, a3))

instance Rankable key => Rankable (Maybe key) where
  rank l = [v| (mk, v) <- l , isNothing mk]:(rankNotNothing l) where 
    rankNotNothing = rank . map (\(mk,v) -> (fromJust  mk , v)) . filter (not . isNothing . fst) 
  
instance Rankable key => Rankable [key] where
  rank l = filter (not . null) (rank [(uncons k, v) | (k,v) <- l])

  
  
  

----------------------------------------------------------------------------------------------------
-- some test inputs (it would be reasonably for "rank" and "genericRank" to produce the same output)

testPhrase :: [Char]
testPhrase = "Hello, world!"

boolTest :: [(Bool,Char)]
boolTest = [ (isLetter c, c) | c <- testPhrase ]

maybeTest :: [(Maybe Char,Char)]
maybeTest = [ (if isLetter c then Just c else Nothing, c) | c <- testPhrase ]

tupleTest :: [((Bool,Char),Char)]
tupleTest = [ ((isLetter c, c), c) | c <- testPhrase ]

listTest :: [(String,Char)]
listTest = [ (w, c) | w <- groupBy (\x y->isLetter x==isLetter y) testPhrase, c <- w ]
