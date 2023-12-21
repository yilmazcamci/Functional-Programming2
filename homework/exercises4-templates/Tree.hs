module Tree where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

{----------- exercise 4.3 -------------}

--leaves :: Tree a -> Int
--nodes  :: Tree a -> Int
--height :: Tree a -> Int
--elems  :: Tree a -> [a]
--isSearchTree :: (Ord a) => Tree a -> Bool

{----------- exercise 4.4 -------------}

member :: (Ord a) => a -> Tree a -> Bool
member x Leaf = False
member x (Node y t s) | x == y = True
                          | x <  y = member x t
                          | otherwise = member x s

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y t s) | x < y = Node y (insert x t) s
                      | x > y = Node y t (insert x s)
                      | otherwise = Node y t s

                      
delete :: (Ord a) => a -> Tree a -> Tree a
delete x Leaf = Leaf
delete x (Node y t s) | x < y = Node y (delete x t) s
                      | x > y = Node y t (delete x s)
delete x (Node y (Node z a b) s) = Node z (delete z (Node z a b)) s 
delete x (Node y t (Node w a b)) = Node w t (delete w (Node w a b)) 
delete x (Node y Leaf Leaf) = Leaf
                    


fromList :: (Ord a) => [a] -> Tree a
fromList l = fromListIntermediate l Leaf where
  fromListIntermediate [] t = t
  fromListIntermediate (x:xs) t = insert x (fromListIntermediate xs t)

{----------- exercise 4.5 -------------}

inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node x t s) = inOrder t ++ [x] ++ inOrder s

fromAscList :: [a] -> Tree a
-- given a list that is already sorted in ascending order
-- and does not contain duplicates, constructs a binary search tree that is balanced—that is:
-- for every node the height of both children should differ by at most one. (Using fromList
-- on a sorted list probably produces tree that is very unbalanced.)

-- We do this as follows: given a Tree Node x t s, x should be the middle of the list and 
-- the first child Tree t should store the front half of the list, the second child s should
-- store the back half of the list.

fromAscList [] = Leaf
fromAscList [x] = Node x Leaf Leaf
fromAscList l | even (length l) = let half = (length l) `div` 2 in Node (l!!half) (fromAscList (take (half) l)) (fromAscList (drop (half + 1) l)) 
              | otherwise = let half = (length l) `div` 2 in Node (l!!(half)) (fromAscList (take (half) l)) (fromAscList (drop (half + 1) l))


-- helper functions for extracting data from nodes in BFS:
content :: Tree a -> [a]
content Leaf = []
content (Node x _ _) = [x]

children :: Tree a -> [Tree a] 
children Leaf = []
children (Node x s t) = [s,t]

breadthFirst :: Tree a -> [a]
breadthFirst t = bfs [] [t] where
  bfs acc []      = acc
  bfs acc (q:qs)  = bfs (acc ++ content q) (qs ++ children q)

{- BONUS: a tree pretty printer; the recursive structure of this function
 - is prety simple, but it is a fiddly function to write if you want it to
 - produce an actually nice tree. -}

{-
layout :: (Show a) => Tree a -> String
layout tree = go "" ("","","") tree
  where
  width = maximum (0:[ length (show e) | e <- elems tree ])
  pad s = let s' = show s in replicate (width-length s') '-' ++ s'
  fill  = replicate width ' '

  --go pre (_,_,preN) Leaf = pre ++ preN ++ "·\n" -- this explicitly draws the leaves
  --go _   _          Leaf = ""                   -- this vertically compresses the tree
  go pre _          Leaf = pre ++ "\n"            -- use more vertical space, but don't draw leaves
  go pre (preR,preL,preN) (Node k lt rt)
    = go (pre ++ preR) (hfill,v_bar,rbend) rt
      ++ (pre ++ preN) ++ pad k ++ junct ++
      go (pre ++ preL) (v_bar,hfill,lbend) lt

  junct = "┤\n"         -- change to "+\n" if no Unicode
  hfill = fill ++ "  "
  rbend = fill ++ "╭─"  -- change to "/-" if no Unicode
  v_bar = fill ++ "│ "  -- change to "| " if no Unicode
  lbend = fill ++ "╰─"  -- change to "\\-" if no Unicode

putTree :: (Show a) => Tree a -> IO()
putTree tree = putStr (layout tree)
-}
