Logging is a process of recording/collecting any (additional) information during application
run time.
Take, for example, the following function that determines the number of elements for which
the given predicate holds.

\begin{code}

countP p (x:xs)
    | p x = countP p xs + 1
    | otherwise = countP p xs
    
\end{code}

Suppose we not only want to determine how many elements are involved, but also want to
know the elements themselves. We could then adjust count as follows.

\begin{code}

countAndLogNM :: (Show a) => (a -> Bool) -> [a] -> (Int, [String])
countAndLogNM _ [] = (0, [])
countAndLogNM p (x:xs)
    | p x = let (c, ls) = countAndLogNM p xs
            in (c+1, show x:ls)
    | otherwise = countAndLogNM p xs
    
\end{code}

    
The disadvantage of this implementation is that the collection of the ’logs’ (in this case the
list of elements for which the predicate holds) is tracked explicitly in the function itself. That
will be different if we use a monads based implementation. Then the monad will internally
ensure that all generated logs are combined and returned.
The next type Logger represents a value (of type a) plus a list of log messages (of type
[String]). The function runLogger can be used to transform a Logger into a tuple.

\begin{code}

data Logger a = Logger a [String]
    deriving Show
    
runLogger :: Logger a -> (a, [String])
runLogger (Logger x logs) = (x, logs)

\end{code}


(a)(3) Show that Logger is a monad by giving an appropriate instance of class Monad for this
type and implementing that class’s two member functions. You don’t need to provide in-
stances for the Functor and Applicative classes, and your implementation of Monad
should not depend on those instances.

\begin{code}

instance Monad Logger where
    -- pure :: a -> Logger a
    return x    = Logger x []
    -- (>>=) :: Logger a -> (a -> Logger b) -> Logger b
    logger >>= f = let (x, logs) = runLogger logger 
                       (y, extralogs) = runLogger (f x)
                   in  Logger y (logs ++ extralogs)
                   
instance Applicative Logger where
    pure x    = Logger x []
    loggerF <*> loggerX = let (f, log) = runLogger loggerF
                              (x, extralog) = runLogger loggerX
                          in Logger (f x) (log ++ extralog)

instance Functor Logger where
    fmap f logger = let (x, log) = runLogger logger in Logger (f x) log
    

\end{code}

(b)(1) Define the primitive logMsg :: (Show a) =>a -> Logger () that logs the pro-
vided object as a message (using show to convert it into a String).

\begin{code}

logMsg :: (Show a) => a -> Logger ()
logMsg x = Logger () [show x]

\end{code}

(c)(3) Write a function countAndLog that is a monadic version of the previously given func-
tion countAndLogNM. First, write down the (new) signature of this function. You should
consider the Logger monad as a black box, and it is therefore not permitted to unpack-
/pack it (by removing/adding the Logger constructor). The only allowed operations
on Loggers are the methods from the monad class, its superclasses, and the primitive
operation logMsg from the previous question.

\begin{code}

countAndLog :: (Show a) => (a -> Bool) -> [a] -> Logger Int
countAndLog p []     = return 0
countAndLog p (x:xs) | p x = do
                        logMsg x
                        countNext <- countAndLog p xs
                        return (1 + countNext)
                     | otherwise = countAndLog p xs
               
                     
test = countAndLog even [1..19]
-- Logger 9 ["2","4","6","8","10","12","14","16","18"]                     
\end{code}
