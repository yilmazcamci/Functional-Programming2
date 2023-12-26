module MyRandomState (
    RandomState,
    mkSeed,
    getSeed,
    putSeed
) -- instance declarations are always exported, and cannot be explicitly named in export list.
where

type Seed = Int 
-- In reality, the Seed implementation in module RandomState 
-- is hidden and inaccessible (i.e. not exported).

mkSeed :: Int -> Seed -- creates a new seed out of an integer value
mkSeed = id

{-
    Below is a toy random number generator, which generates a random number (Int)
    and a new Seed (Int)
-}

nextRandom :: Int -> Seed -> (Int, Seed) -- generates a (pseudo) random number n, and a new seed
nextRandom n s = ((s * 50) `mod` n, (s ^ 2) `mod` 5000)

{-
    If you want to use the nextRandom function multiple times, you need to pass the seed explicitly
    all the time. Using the state monad prevents this: the monad operations ensure that the seed
    is properly passed in the background. We can use the following type to represent this monad
    together with an accessor for the wrapped function:
-}
newtype RandomState a = RS {runState :: Seed -> (a,Seed)}
{-
    The State monad in reality is type-generic over Seed:
    
newtype State s a = State {runState :: s -> (a,s)}
    
    But our Seed = Int is sufficient for now. The main goal is to illustrate the instance
    declarations for Functor, Applicative and Monad.
-}
{-
    In addition, we have 2 primitives for retrieving the seed
    and restoring a (possibly changed) seed. We present their implementation.
-}
getSeed :: RandomState Seed
getSeed = RS $ \s -> (s,s)

putSeed :: Seed -> RandomState()
putSeed s = RS $ \_ -> ((), s)

instance Functor RandomState where
    -- fmap :: (a -> b) -> RandomState a -> RandomState b
    fmap f (RS g) = RS $ \s 
        -> let (xa, ss) = g s
           in (f xa, ss)


instance Applicative RandomState where
    -- pure :: a -> RandomState a
    -- (<*>) :: RandomState (a -> b) -> RandomState a -> RandomState b
    pure x = RS $ \s -> (x,s)
    (RS f) <*> (RS g) = RS $ \s 
        -> let (fa, fs) = f ss
               (sa, ss) = g s
           in (fa sa, fs)
           

instance Monad RandomState where
    -- (>>=) :: RandomState a -> (a -> RandomState b) -> RandomState b
    (RS x) >>= f = RS $ \s 
        -> let (xa, ss) = x s
           in  runState (f xa) ss
        
        
