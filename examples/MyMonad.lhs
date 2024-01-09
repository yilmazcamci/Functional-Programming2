In order to avoid ambiguity of Functor, Monad, Applicative and
all accompanying functions:
\begin{code}
import Prelude (Either(Left,Right), IO, Bool, Char, String, Int, (-), ($), (.), otherwise, (<=), id, flip, const, Maybe(Nothing,Just), map)

import Data.List (uncons)


\end{code}

Functor: 

\begin{code}

class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b
    
    (<$>) :: (a -> b) -> f a -> f b
    (<$>) = fmap
    
    
(<$) :: Functor f => a -> f b -> f a
(<$) = fmap . const
-- Replaces all locations in second argument with first argument's value.

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

\end{code}

Instances should satisfy Functor Laws:

Identity:       fmap id = id

Homomorphism:   fmap (f . g) = fmap f . fmap g
    
Some canonical instances:

\begin{code}

instance Functor [] where
    fmap = map

instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)
    
instance Functor (Either a) where
    fmap _ (Left x)  = Left x
    fmap f (Right y) = Right (f y)

newtype Parser i a = P {runP :: [i] -> Maybe (a, [i])}

instance Functor (Parser i) where
    fmap f p = P $ \ inp ->
        case runP p inp of
            Nothing -> Nothing
            Just (out, inp') -> Just (f out, inp')

instance Functor IO where
    -- fmap :: (a -> b) -> IO a -> IO b
    fmap f act = do 
        x <- act
        return (f x)    

\end{code}

Note: since do-notation is syntactic sugar for bind- and return expressions,
the above instance declaration for IO depends on the instance Monad IO directly.
In general, if a Monad instance exists, then a unique Applicative, and therefore
Functor instance can be derived;
however it is often implementationally suboptimal to define Functor operations via Monad operations:

fmap f act = do
    x <- act
    return (f x)
           = pure f <*> act
           = act >>= (\x -> pure (f x))
           
The difference between Applicative and Functor is that

- Functors f have a mapping between f a -> f b, but only for every function a -> b.

- Applicatives f have a mapping between f a -> f b, for generic f (a -> b)

Then comes Monad, which has

- A mapping f a -> f b for every a -> f b

Every pure h for h :: a -> b is of type f (a -> b). This  means we can define fmap in pure and <*>,
so every Applicative has a unique Functor instance.

However, not every g :: f (a -> b) is of the form fmap h for some h :: a -> b. This makes
Applicative strictly more expressive than Functor.

If g :: f (a -> b) for an applicative f, then
(x -> g . pure x) :: a -> f b, and return = pure, so every Monad has a unique Applicative instance.

However, not every a -> f b is of the form (x -> g . pure x) for some g :: f (a -> b)


A good example is:
    
    join :: f (f a) -> f a
    
Here, pure x :: f (f (f a)), so g :: f (f (f a) -> f a), so if we had such a g, it would be of
the unique form
    
    pure join
   
Which requires us to have a join first.
In fact, it appears that join is thé hard filter that distinguished Applicatives from Monads. Because,
it is possible to define the bind operation in Monad in terms of pure and join:

(>>=) :: m a -> (a -> m b) -> m b
mx >>= f = join (f <$> mx)

On the other hand, we can define join in terms of >>= too:

join :: m (m a) -> m a
join = (id =<<)


Applicative

A structure intermediate between a functor and a monad (technically, a strong lax monoidal functor). Compared with monads, this interface lacks the full power of the binding operation >>=, but

- it has more instances.
    
- it is sufficient for many uses, e.g. context-free parsing, or the Traversable class.

- instances can perform analysis of computations before they are executed, and thus produce shared optimizations.

\begin{code}

class (Functor f) => Applicative (f :: * -> *) where
    pure  :: a -> f a
    
    
    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = liftA2 id
    -- where id :: (a -> b) -> a -> b, clearly
    
    
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c 
    liftA2 f u v = f <$> u <*> v
    -- where we see: (f <$>) :: f a -> f (b -> c)
    

(*>) :: Applicative f => f a -> f b -> f b
u *> v = (id <$ u) <*> v
-- Sequence actions, discarding the value of the first argument.
-- For example,
-- [1, 2, 3] *> ['a', 'b'] = 
-- (id <$ [1,2,3]) <*> ['a','b']  =
-- (fmap (const id) [1,2,3]) <*> ['a','b'] =
-- [id, id, id] <*> ['a','b'] = [a','b', a','b', a','b']

-- Or, as another example, consider the parsers:

item :: Parser Char Char 
item = P uncons

seconditem = item *> item
-- This works, because
-- item *> item = 
-- (id <$ item) <*> item = 
-- (fmap (const id) item) <*> item = 
-- (P $\inp -> (id,rem) where (out, rem) = runP item inp) <*> item =
-- P $\inp -> (id out', rem') where (out', rem') = runP item rem where (_, rem) = runP item inp

-- Note: the above only make sense when both parsers do not fail, i.e. the string has 2 or more
-- elements.

-- And we clearly see now, how (const id) effectively discards the result of the first applicative
-- but the context of the first applicative remains (either the length (=structure/context) of the
-- containing list, or the passed extra state rem in the parser.

    
(<*) :: Applicative f => f a -> f b -> f a
u <* v = liftA2 const u v
-- Sequence actions, discarding the value of the second argument.
-- Using the item example, it is now intuitively clear that item <* item should give the first
-- element of the input, but the remainder rem' that is present after two item applications.
-- Let's make this rigorous: again, assume 2 or more elements (hence item does not fail):


-- item <* item = 
-- LiftA2 const item item = 
-- const <$> item <*> item = 
-- fmap const (P $uncons) <*> (P $uncons) = 
-- (P $\inp -> (const x, xs) where (x,xs) = uncons inp) <*> (P $uncons) =
-- P $\inp -> (const x xx, xss) where (xx, xss) = uncons xs where (x,xs) = uncons inp =
-- P $\inp -> (x, xss) where (_,xss) = uncons xs where (x,xs) = uncons inp

-- And we see that this is indeed the case.

\end{code}

A minimal complete definition must include implementations of pure and of either <*> or liftA2. If it defines both, then they must behave the same as their default definitions:

(<*>) = liftA2 id

liftA2 f x y = f <$> x <*> y

Applicative instances should satisfy the Applicative Laws:

Identity:       pure id <*> v = v

Composition:    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

Homomorphism:   pure f <*> pure x = pure (f x)

Interchange:    u <*> pure y = pure ($ y) <*> u

Some instances:

\begin{code}

instance Applicative [] where
    pure = (:[])
    lf <*> lx = [f x | f <- lf, x <- lx]
    -- There are two possibilities given identity (which is just the Functor lawy)
    -- , but only one satisfies all other laws:
    -- (1) [f x | f <- lf, x <- lx], or
    -- (2) [f x | x <- lx, f <- lf]
    
    -- Composition implies:
    -- for g, h :: a -> b, e, f :: b -> c, x:: a
    -- pure (.) <*> [e,f] <*> [g, h] <*> [x] = 
    -- (1) [. e, . f] <*> [g, h] <*> [x]  =
    -- (1) [g.e, h.e, g.f, h.f] <*> x = 
    -- (1) [g(e(x)), ... h(f(x))]
    
    --

instance Applicative Maybe where
    pure = Just
    Nothing  <*> _ = Nothing
    _  <*> Nothing = Nothing
    Just f <*> Just x = Just $ f x
    
instance Applicative (Either a) where
    pure = Right
    Left a <*> Right x = Left a
    Right f <*> Left a = Left a
    Right f <*> Right x = Right $ f x

instance Applicative (Parser i) where
    pure x = P $ \input -> Just (x,input)
    p1 <*> p2 = P $ \input ->
        case  runP p1 input of
            Nothing -> Nothing
            Just (parse1,out1) -> case runP p2 out1 of
                Nothing -> Nothing
                Just (parse2,out2) -> Just (parse1 parse2, out2)
    

instance Applicative IO where
    pure = return
    act1 <*> act2 = do  
        f <- act1
        x <- act2
        return (f x) -- Note that this implementation holds in general 
                     -- for any Monad (since Monads are also Applicatives)
        
    
newtype StateT s m a = ST {runS :: s -> m (a, s)}

  
--instance (Monad s, Monad m) => Functor StateT s m
--    fmap f st = 
     

\end{code}

Monad

\begin{code}

class (Applicative m) => Monad (m :: * -> *) where
    return :: a -> m a
    return = pure
    
    (>>=) :: m a -> (a -> m b) -> m b
    mx >>= f = join (f <$> mx)
    
    join :: m (m a) -> m a
    join = (id =<<)
    
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(>>) :: Monad m => m a -> m b -> m b
mx >> my = mx >>= (\_ -> my)


{-
          = do 
    x <- mx
    y <- my
    return y
-}

(>=>)   :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g =  \x -> (f x >>= g)

-- This works, since
-- f x :: m b
-- g :: b -> m c

\end{code}

More derived functions:

\begin{code}

liftM :: Monad m => (a -> b) -> m a -> m b
liftM = fmap

-- Note: this is a monomorphized version, and should in general hold
-- for Traversables t rather than only lists []
mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM f [] = pure []
mapM f (x:xs) =
    f x >>= \y ->
    mapM f xs >>= \ys ->
    return (y:ys)
-- correct:
-- mapM :: (Traversable t, Monad m_ => (a -> m b) -> t a -> m (t b)

mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ f xs = mapM f xs >>= const (pure ())

foldM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldM _ z [] =  pure z
foldM g z (x:xs) =
    g z x >>= \y ->
    foldM g y xs

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ []     = pure []
filterM p (x:xs) =
    p x >>= \verdict ->
    filterM p xs >>= \ys ->
    if verdict then return  (x:ys) else return ys

replicateM :: (Applicative m) => Int -> m a -> m [a]
replicateM n x | n <= 0 = pure []
               | otherwise = (:) <$> x <*> replicateM (n - 1) x

sequence :: (Monad m) => [m a] -> m [a]
sequence [] = pure []
sequence (x:xs) =
    x >>= \y ->
    sequence xs >>= \ys ->
    pure (y:ys)

sequence_ :: (Monad m) => [m a] -> m ()
sequence_ [] = pure ()
sequence_ (x:xs) =
    x >>
    sequence xs >>
    pure ()
-- Although we can also define a sequence for Applicatives:

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

\end{code}

The monad Laws are, expressed in the Kleisi operator (>=>) and return:

Left Identity: for all f :: (b -> m c)

    return >=> f = f

Right Identity: for all f :: (b -> m c)

    f >=> return = f

Associativity:

    f >=> (g >=> h) = (f >=> g) >=> h
    
In terms of (>>=) and return, this writes as:

Left Identity: for all f :: (b -> m c)

    return >=> f = f

Right Identity: for all f :: (b -> m c)

    f >=> return = f

Associativity:

    f >=> (g >=> h) = (f >=> g) >=> h






