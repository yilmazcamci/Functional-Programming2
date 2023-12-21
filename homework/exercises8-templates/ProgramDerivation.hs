module Expression where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

skewed :: Integer -> Tree ()
skewed 0 = Leaf
skewed n = Node () (skewed (n-1)) Leaf

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x lt rt) = inorder lt ++ [x] ++ inorder rt

{-
Derive
  inorderCat t xs = inorder t ++ xs
-}

inorderCat :: Tree a -> [a] -> [a]
inorderCat Leaf xs = xs
inorderCat (Node x lt rt) xs = inorderCat lt (x:(inorderCat rt xs))
-- inorderCat t xs = inorder t ++ xs -- TODO: make me more efficient.

-- Derivation of this definition (by equational reasoning)
-- Base Case:
-- inorderCat Leaf xs = inorder Leaf ++ xs = [] ++ xs = xs
-- Inductive Case:
-- inorderCat (Node x lt rt) xs 
-- = inorder (Node x lt rt) ++ xs
-- = inorder lt ++ [x] ++ inorder rt ++ xs
-- = inorder lt ++ x:(inorder rt ++ xs)
-- = inorder lt ++ x:(inorderCat rt xs)
-- = inorderCat lt (x:(inorderCat rt xs))

inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- TODO: make me more efficient, too
elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt

elemsCat :: Tree a -> [a] -> [a]
elemsCat Leaf xs = xs
elemsCat (Node x lt rt) xs = x: elemsCat lt (elemsCat rt xs)

-- We define elemsCat by the relation:
-- elemsCat t xs = elems t ++ xs
-- Then we can derive an implementation for elemsCat
-- "using induction":
-- Base Case:
-- elemsCat Leaf xs = [] ++ xs = xs
-- Inductive Case:
-- elemsCat (Node x lt rt) xs 
-- = elems (Node x lt rt) ++ xs
-- = x : (elems lt ++ elems rt) ++ xs
-- = x : elems lt ++ (elems rt ++ xs)
-- = x: elemsCat lt (elemsCat rt xs)

elems' :: Tree a -> [a]
elems' t = elemsCat t []
