(a)(3) Consider the following data type:

\begin{code}

data MinMax a = Undef | MinMax a a
    deriving Show

\end{code}

Make MinMax an instance of Functor.

\begin{code}

instance Functor MinMax where
    fmap f Undef = Undef
    fmap f (MinMax x y) = MinMax (f x) (f y)

\end{code}

(b)(4) The monoid class from Haskell’s standard library is defined as follows (for convenience
we have included the operation <> which is actually defined in a separate Semigroup
class)

class Monoid m where
    mempty :: m
    (<>) :: m -> m -> m
 

This class models a type m with an (associative) operation <> which combines two ele-
ments into one, and a unit element mempty.
Make MinMax a an instance of Monoid in which <> returns the minimum and maximum
of the two arguments. For example:

    MinMax 1 4 <> MinMax 5 7 = MinMax 1 7
    MinMax 3 3 <> MinMax 2 2 = MinMax 2 3
    MinMax 1 4 <> Undef = MinMax 1 4
    Undef <> MinMax 6 6 = MinMax 6 6

\begin{code}

instance (Ord a) => Semigroup (MinMax a) where
    Undef      <> m     = m
    m          <> Undef = m
    MinMax x y <> MinMax v w = MinMax (min x v) (max y w)


instance (Ord a) => Monoid (MinMax a) where
    mempty = Undef

\end{code}    
    
    
(c)(4) Now consider the following data type for (internally labeled) binary trees:

\begin{code}

data BTree a = Leaf | Node (BTree a) a (BTree a)

\end{code}

Make BTree an instance of Foldable. Hint: it is probably easier to define foldMap than
foldr.

\begin{code}

instance Foldable BTree where
    foldMap :: (Monoid m) => (a -> m) -> BTree a -> m
    foldMap f  Leaf       = mempty
    foldMap f (Node t1 x t2) = (foldMap f t1) <> f x <> (foldMap f t2)

\end{code}

(d)(2) Use foldMap to define a function 

minMaxBtree :: Ord a ⇒ BTree a → MinMax a 

that determines the minimum and the maximum elements of a binary tree using a single
traversal (that is, with a single call to foldMap).

\begin{code}

minMaxBtree :: Ord a => BTree a -> MinMax a
minMaxBtree = foldMap (\x -> MinMax x x) 

\end{code}
