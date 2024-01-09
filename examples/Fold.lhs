(Finite) lists support a fold over their values:

\begin{code}

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op e [] = e
foldr op e (x:xs) = x `op` foldr op e xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op acc [] = acc
foldl op acc (x:xs) = foldl op (acc `op` x) xs

-- strict foldl: accumulator immediately evaluated
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' op acc [] = acc
foldl' op acc (x:xs) = foldl op ($! acc `op` x) xs

\end{code}

foldr preserves the structure of the list, in the sense that 
the evaluated expression just has `op` in place of : and e in
place of [].

On the other hand, foldl inverts the association order:

\begin{code}

foldr (<>) empty (1 : (2 : (3 : [])) = (1 <> (2 <> (3 <> empty)))
foldl (<>) empty (1 : (2 : (3 : [])) = (((empty <> 1) <> 2) <> 3)

\end{code}



If op :: a -> a -> a, e :: a and (a, e, op) form a monoid, op is 
associative and the value of this expression should be the same.

There is, of course, a typeclass for monoids. It is built 
incrementally upon the typeclass for semigroups:

\begin{code}

class Semigroup m where
    (<>) :: m -> m -> m
 
class Semigroup m => Monoid m where
    mempty :: m

\end{code}

In practice, many types can be made into a semigroup/monoid in multiple ways.
To allow for multiple instances, use wrappers:

\begin{code}
-- WRONG: you can only do this once.
instance Num a => Semigroup a where
    (<>) = (+)
instance Num a => Monoid a where
    mempty = 0
    
-- CORRECT:

newtype Sum     a = Sum {getSum :: a}
newtype Product a = Product {getProduct :: a}
newtype Min     a = Min {getMin :: a}

instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum $ x + y

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    
instance Num a => Semigroup (Product a) where
    Product x <> Product y = Product $ x + y

instance Num a => Monoid (Product a) where
    mempty = Product 0
    
instance Ord a => Semigroup (Min a) where
    Min x <> Min y = Min $ min x y

instance (Bounded a, Ord a) => Monoid (Min a) where
    mempty = Min minBound

-- Another interesting Monoid:
newtype Endo a = Endo {apply :: a -> a}

instance Monoid (Endo a) where
    mempty = Endo id
    Endo f <> Endo g = Endo $ f . g

-- Is a group when every endomorphism is bijective

\end{code}

Given a Monoid m, we can define one well-defined fold:

\begin{code}

fold :: Monoid m => [m] -> m
fold = foldr (<>) mempty

foldMap :: Monoid m => (a -> m) -> [a] -> m
foldMap f [] = mempty
foldMap f (x : xs) = f x <> foldMap f xs
-- foldMap = foldr (\x y -> f x <> y) mempty

\end{code} 

Note that this is not always desirable in the case of lists, 
because the evaluation order may have a significant impact on 
the complexity of the evaluation.

Example: ([a],(++),[]) is a monoid for all a :: Type. 
Therefore, the fold order does not matter. Consider two
implementations for concatenating lists of lists (join,
in the list Monad)

\begin{code} 

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

concat1 :: [[a]] -> [a]
concat1 = foldr (++) []

concat2 :: [[a]] -> [a]
concat2 = foldl (++) []

\end{code}

we notice, given the above implementation of (++), that:

concat1 [[1],[2],[3]] = 
[1] ++ ([2] ++ [3])   =
1 : ([2] ++ [3])      = 
1 : 2 : [3]

concat2 [[1],[2],[3]] = 
([1] ++ [2]) ++ [3]   =
[1,2] ++ [3]          =
1 : ([2] ++ [3])      = 
...
1 : 2 :[3]

We notice that concat2 will concatenate the leftmost lists 
first, thereby requiring us to traverse the accumulator
entirely every time we apply 
      
    acc ++ x

This is very inefficient and leads to a complexity of O(n^2),
compared to O(n) for concat1 (where n is the number of lists to 
be concatenated).

Different implementations for (++) may lead to different choices
between foldl and foldr, but this definition for (++) is the 
most common one I've seen.

Foldable

We now generalize from [] to arbitrary containers, which can be an
instance of the class Foldable if they support fold when their elements 
are part of a monoid:

\begin{code}

class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    -- Should be semantically identical (and by default) to:
    -- foldr f z = foldr f z . toList
    
    foldl :: (b -> a -> b) -> b -> t a -> b
    -- Semantically (and by default):
    -- foldl f z = foldl f z . toList
    
    fold :: Monoid m => t m -> b
    -- default:
    -- fold = foldr
    
    foldMap :: Monoid m => (a -> m) -> t a -> b
    -- default:
    -- foldMap f t = foldr (\x y -> f x <> y) mempty t
    
\end{code}  

Some things that are obvious once you know them:

    -- Minimal complete definition: foldMap | foldr
    -- foldMap is easiest most of the time.
    
    -- foldr can be derived from foldMap, since Monoid [a] with (++):
    -- foldr f z = foldr f z . toList
    -- toList = foldMap (:[])
    
    -- foldMap can be derived from fold, which is just foldr due 
    -- to associativity of Monoid m:
    -- fold = foldr
    -- foldMap





There are many other functions in this typeclass, 
notably those for non-empty structures and those with strict evaluation order:

\begin{code}
    null :: t a -> Bool
-- Test whether structure is empty. Default implemenation is left-associative and lazy 
-- in both the initial element and accumulator (Hackage)
-- Expected to terminate even for infinite structures. So, clearly wrong:
-- null t = length t /= 0

    length :: t a -> Int
-- returns size/length of a finite structure as Int. Default impl. just counts elements.
-- But you may have designed a container that has a more efficient way than just
-- element-by-element counting, i.e. a tree with subtree sizes kept in each node. Then,
-- provide a specialized impl.

    toList :: t a -> [a]
-- List of elements of a structure, from left to right.
-- i.e. default:
-- toList = foldMap (:[])

    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldr' f z = foldr' f z . toList

\end{code} 
This is an interesting function, since we see that for lists it does not sense to make right-associative evaluation strict: there is no speed gain, and we also discard the benefits of lazy evaluation. Moreover, accessing the rightmost element of a list requires O(length l) time, making this almost always a poor choice.

According to the documentation (Hackage), there is sense for some data structures, though not for lists:

"
    foldr' is a variant of foldr that performs strict reduction from right to left, i.e. starting with the right-most element. The input structure must be finite, otherwise foldr' runs out of space (diverges).

    If you want a strict right fold in constant space, you need a structure that supports faster than O(n) access to the right-most element, such as Seq from the containers package.

    This method does not run in constant space for structures such as lists that don't support efficient right-to-left iteration and so require O(n) space to perform right-to-left reduction. Use of this method with such a structure is a hint that the chosen structure may be a poor fit for the task at hand. If the order in which the elements are combined is not important, use foldl' instead.
"

Continu
\begin{code}


    foldl' :: (b -> a -> b) -> b -> t a -> b 
    -- left-associative, strict application of the operator
    -- default, and semantically should satisfy:
    -- foldl' f z = foldl' f z . toList
    
    foldr1 :: (a -> a -> a) -> t a -> a 
    -- right-associative
    -- no base case: assumes nonempty t structure, else will raise runtime exception

    foldl1 ::
    -- left-associative
    -- no base case: assumes nonempty t structure, else will raise runtime exception

    elem :: a -> t a -> Bool
    -- obvious. default is recursive search, may be optimized for many lookup-oriented datastructures
    
    maximum :: Ord a => t a -> a
    -- Assumes non-empty structure! Else runtime exception
    
    minimum :: Ord a => t a -> a
    -- Assumes non-empty structure! Else runtime exception
    
    product :: Num a => t a -> a
    -- product of the elements.
    -- default: fold (*) 1
    -- again, you may have a datastructure that has more optimal ways
    -- to compute this.
    
    sum :: Num a => t a -> a
    -- sum of the elements.
    -- default: probably smth like fold (+) 0

\end{code}

There are many more specialized folds! Too many to name here, really. Important to know is that
there are monadic (action)/folds, applicative folds, alternative folds. In particular, we can 
generalize mapM_, sequence, from the Monads summary to general foldables:

\begin{code}

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b

-- Right-to-left monadic fold over the elements of a structure.

-- Given a structure t with elements (a, b, c, ..., x, y), the result of a fold with an operator function f is equivalent to:

foldrM f z t = do
    yy <- f y z
    xx <- f x yy
    ...
    bb <- f b cc
    aa <- f a bb
    return aa -- Just @return z@ when the structure is empty


mapM_ :: (Foldable t, Monad m_ => (a -> m b) -> t a -> m ()

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m () 
-- forM_ is mapM_ with arguments flipped

sequence_ :: (foldable t, Monad m) => t (m b) -> m ()



-- traverse_ is just like mapM_ but generalized to Applicative actions:
traverse_ :: (Foldable t, Applicative m) => (a -> m b) -> t a -> m ()

for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f () 
-- for_ is traverse_ with arguments flipped

concat :: Foldable t => t [a] -> [a]
concat = mconcat


foldlM, foldrM, msum, asum, ...
\end{code}

These are functions that discard the value of the monadic operation. That is why it is not necessary to have a function that "preserves the structure of the container" (i.e. a traversing function): we only need a folding function (such as foldr, foldMap).

However, we cannot yet do this for mapM: for this, we need Traversables:

\begin{code}

mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)

\end{code}

Why? Foldable only allows us to fold a datastructure, so we go transform a container into
more elementary type. There is no operation "preserving the structure" that can be used to preserve the structure of the container. For this, we need a "traversing" function, which is defined by the 
typeclass Traversable.

Some instances of Foldable:

\begin{code}

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Foldable Tree where
    foldMap : : Monoid m => (a -> m) -> Tree a -> m
    foldMap f ( Leaf x) = f x
    foldMap f (Node l r ) = foldMap f l <> foldMap f r

\end{code}

Traversable

A motivating example: Mapping over a functon that may fail (i.e. a -> Maybe b) 
(you can sense the Monad hanging from the ceiling) over a list:

\begin{code}

traverse :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse f [] = pure []
traverse f (x:xs) = (:) <$> f x <*> traverse f xs 
    -- All that is needed: 
    -- 1. An applicative instance for Maybe.
    -- 2. a functor instance for []

\end{code}

This digresses to the class Traversable:

\begin{code}

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    -- traverse g = sequenceA . fmap g
    
    sequenceA :: Applicative f => t (f a) -> f (t a)
    -- sequenceA = traverse id

\end{code}
It is clear that minimally, we need either sequenceA | traverse to be implemented.
As traverse requires two passes over te container, one by fmap g and the other by sequenceA,
a direct implementation of traverse is generally preferred over sequenceA.

\begin{code}
    
-- Some exemplifying instances:


instance Traversable [ ] where
    traverse : : Applicative f ⇒ (a → f b) → [ a ] → f [b]
    traverse g [ ] = pure [ ]
    traverse g (x : xs ) = (:) <$> g x <*> traverse g xs
    
instance Traversable Tree where
    traverse : : Applicative f ⇒ (a → f b) → Tree a → f (Tree b)
    traverse g ( Leaf x) = Leaf <$> g x
    traverse g (Node l r ) = Node <$> traverse g l <*> traverse g r

\end{code}

Traverse can be used to generalize mapM:

\begin{code}

askUser :: String -> IO String
askUser question = do
    putStrLn question
    answer <- getLine
    pure answer
    
intro1 = mapM askUser ["What is your name?", "Do you like Haskell?"]
intro2 = traverse askUser ["What is your name?", "Do you like Haskell?"]

\end{code}

It also generalizes Functor, as follows:

\begin{code}

newtype Identity a = Identity {getId :: a}

instance Functor Id where
    fmap (Id x) = Id (f x)
    
instance Applicative Id where
    pure = Id
    Id f <*> Id x = Id $ f x
    
-- Then:

amap :: Traversable f => (a -> b) -> f a -> f b -- our map equivalent
amap g = getId . traverse (Id . g)

-- Check the types first:
-- Id . g :: a -> Id b
-- traverse (Id . g) :: Traversable t => t a -> Id (t b)
-- getId . traverse (Id . g) :: t a -> t b

-- So, we can use Id to extract a unique t b for every Id (t b), and that is
-- Why we can go back from Id to value (there is no information lost by discarding the context).

-- we require fmap = amap for Traversables, obviously.

\end{code}

It also generalizes Foldables. Every Traversable is a Foldable, where
we provide an implementation for foldMap as follows:

\begin{code}

newtype Const c a = Const {getConst :: c}

instance Functor (Const c) where
    fmap _ v = v

instance Monoid c => Applicative (Const c) where
    pure _ = Const mempty
    Const x <*> Const y = Const (x <> y)

afoldMap :: (Traversable f, Monoid m) => (a -> m) -> f a -> m
afoldMap g = getConst . traverse (Const . g)

-- Check the types first:
-- Const . g :: a -> (Const m) a
-- traverse (Const . g) :: f a -> (Const m) (f a)
-- getConst . traverse (Const . g) :: f a -> m

-- If you think about it, you can see that the (<>) - applications
-- all happen in the first argument of Const while traversing the 
-- structure, while nothing really happens in the second argument, and
-- we discard that in the end.

\end{code}


Ad hoc traversals: Traversal and Lens.

Traversable t defines one particular way of visiting the elements of a container:
in particular, traverse needs to visit all elements in a container in such a way that its
derived amap and afoldMap implementations conform with the Functor and Foldable instances
for t.

Sometimes, we want to traverse in a particular way: consider the example of arithmetic expressions:

\begin{code}

Expr a = Lit a 
    | Add (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    deriving (Show, Eq)
    
lits :: (Applicative f) => (a -> f a) -> Expr a -> f (Expr a)
lits g (Lit x)   = Lit <$> g x
lits g (Add x y) = Add <$> lits g x <*> lits g y
lits g (Mul x y) = Mul <$> lits g x <*> lits g y

-- sum of all literals in an expression:
sumLits :: (Num a) => Expr a -> a
sumLits = getSum . getConst . lits (Const . Sum)

-- number of literals:
countLits :: Expr a -> a
countLits = getSum . getConst . lits (Const . Sum . (const 1))

\end{code}

We introduce a type synonym for ad-hoc traversals: Like we
have

\begin{code}
data Ordering = LT | EQ | GT

class Ord a where
    compare :: a -> a -> Ordering

type Comparator a = a -> a -> Ordering
-- For ad-hoc (higher-order) comparison functions

sort   :: Ord a => [a] -> [a]
sortBy :: Comparator a -> [a] -> [a]

\end{code}

We introduce Traversal:

\begin{code}
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f (t b)
type Traversal' s a = Traversal s s a a

\end{code}
Note that since f is not present as a type parameter, we quantify it: in particular, this 
use of the forall (there are multiple specific, language-defined use cases) serves to require,
informally speaking,
that for all f that are instances of Applicative, a function (a -> f a) -> s -> s f needs to be defined.

In particular, this means that Traversal cannot depend on any information about the particular Applicative f, it is only allowed to make use of the Applicative operators.

Wikibooks says it as follows:

	The forall f. on the right side of the type declaration means that any Applicative can be used to replace f. That makes it unnecessary to mention f on the left side, or to specify which f to pick when using a Traversal. 

We can use Traversals as maps too:

\begin{code}

mapOf :: Traversal' s a -> (s -> s

\end{code}
