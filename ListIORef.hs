module ListIORef where

import Data.IORef

{-

data IORef a

newIORef :: a -> IO (IORef a)\

readIORef :: IORef a -> IO a

writeIORef :: IORef a -> a -> IO ()

modifyIORef :: IORef a -> (a -> a) -> IO () 

-}

copy : : IORef a -> IORef a -> IO ()
copy x y = do 
    val <- readIORef y
    writeIORef x val
    
swap : : IORef a -> IORef a -> IO ()
swap x y = do 
    a <- readIORef x
    b <- readIORef y
    writeIORef x b
    writeIORef y a

ListRef a = IORef (List a)
List a  = Nil | Cons (ListRef a)-- see what we're doing here ;)


last :: ListRef a -> IO (ListRef a) 
-- returns a reference to the last element of the list
last ref = do
    val <- readIORef ref
    case val of
        Nil -> pure ref
        Cons x ys -> do
            

append :: ListRef a -> ListRef a -> IO ()
append ref1 ref2 = do
    val1 <- readIORef ref1
    
