
\begin{code}
import Data.List (partition)
import Control.Monad

newtype Seed = Seed Int -- In reality, representation is hidden
    deriving Show
    
mkSeed :: Int -> Seed 
mkSeed = Seed        -- creates a new seed out of an integer value. 
                     -- In reality, implementation is hidden


randomInt :: Seed -> (Int, Seed) -- Implementation hidden in reality (and probably "more random")
randomInt (Seed seed) = (seed' `div` 0x10000, Seed seed')
  where 
    seed' = (a * seed + c) `mod` m
    m = 2^31
    a = 1103515245
    c = 12345
  
nextRandom :: Int -> Seed -> (Int, Seed)
nextRandom n s = (i `mod` n, s')
    where
        (i,s') = randomInt s
      
  
data RandomState a = RS (Seed -> (a, Seed))

runState :: RandomState a -> Seed -> (a, Seed)
runState (RS f) = f

getSeed :: RandomState Seed
getSeed = RS $ \s -> (s,s)

putSeed :: Seed -> RandomState()
putSeed s = RS $ \_ -> ((), s)

\end{code}

To prove that RandomState is indeed a monad we have to provide instances for the classes
Functor, Applicative, and Monad. In addition, we have 2 primitives for retrieving the seed
and restoring a (possibly changed) seed. We present their implementation



(a)(2) Show that RandomState is a Functor, by giving an appropriate instance. Here it is still
permitted to use the representation of RandomState, but not the instances of Applicative
or Monad.


\begin{code}

instance Functor RandomState where
    fmap f rs = RS $ \s -> 
        let (x,s') = runState rs s in (f x, s')
        
        
instance Applicative RandomState where
    pure x = RS $ \s -> (x, s)
    rf <*> rx = RS $ \s -> 
        let (f, s') = runState rf s
            (x, s'')= runState rx s'
        in  (f x, s'')
        
instance Monad RandomState where
    return  = pure
    r >>= f = RS $ \s -> 
        let (x, s') = runState r s
        in  runState (f x) s'
        
\end{code}

(b)(2) Define a function
getRandom :: Int -> RandomState Int
which, for a given value 𝑛, yields a random number 𝑟 with 0 ≤ 𝑟 < 𝑛.


\begin{code}

getRandom :: Int -> RandomState Int
getRandom n = do 
    seed <- getSeed
    let (i,nextSeed) = nextRandom n seed
    putSeed nextSeed
    return i

\end{code}

(c)(2) Define a function

randomList :: Int -> Int -> RandomState [Int]

which applied to two integers, say 𝑠 and 𝑚, yields a list of size 𝑠 consisting of random
numbers between 0 and 𝑚.


\begin{code}

randomList :: Int -> Int -> RandomState [Int]
randomList s m = replicateM s (getRandom m)

\end{code}

(d)(3) Consider the following definition of the well-known quickSort function:
\begin{code}

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    let (l,r) = partition (< x) xs in
        quickSort l ++ [x] ++ quickSort r

\end{code}

A problem with this function is that it is inefficient if the given list is already (almost)
sorted. For example, the call quickSort [1..100000] will take a lot of time.
To improve this, we no longer use the first element as a pivot, but choose a random
element from the input list. The partitioning then takes place on the basis of this random
element. Implement the function

randomQuickSort :: Ord a => [a] -> RandomState [a]

Using the RandomState monad, this function generates a random position first, selects
the element at that position, and partitions the remaining elements. This is followed by
the recursive calls of the sort function analogous to the original quickSort

\begin{code}

randomQuickSort :: Ord a => [a] -> RandomState [a]
randomQuickSort [] = pure []
randomQuickSort xs = do 
    r <- getRandom (length xs)
    let y:ys = drop r xs
        (ls,lg) = partition (< y) (take r xs ++ ys)
    sl <- randomQuickSort ls
    sr <- randomQuickSort lg
    return (sl ++ [y] ++ sr)

\end{code}

(e)(2) Write a function

testSort :: Int -> Int -> Bool

that, for given inputs 𝑠 and 𝑚, generates a list of 𝑠 random elements each with maximum
value 𝑚, then sorts this list (using randomQuickSort) and checks the result using the
predicate ordered :: Ord a => [a] -> Bool. You can use the value 42 as input for the
mkSeed function

\begin{code}

ordered :: Ord a => [a] -> Bool
ordered [] = True
ordered [x] = True
ordered (x:y:zs) = x <= y && ordered (y:zs)

testSort :: Int -> Int -> Bool
testSort s m = fst (runState randomTest (mkSeed 42)) where
    randomTest = randomList s m >>= randomQuickSort >>= (pure . ordered)
    
\end{code}



