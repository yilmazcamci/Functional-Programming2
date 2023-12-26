module ListIORef where

import Data.IORef

{-

data IORef a

newIORef :: a -> IO (IORef a)

readIORef :: IORef a -> IO a

writeIORef :: IORef a -> a -> IO ()

modifyIORef :: IORef a -> (a -> a) -> IO () 

-}



copy :: IORef a -> IORef a -> IO ()
copy x y = do 
    val <- readIORef y
    writeIORef x val
    
swap :: IORef a -> IORef a -> IO ()
swap x y = do 
    a <- readIORef x
    b <- readIORef y
    writeIORef x b
    writeIORef y a

type ListRef a = IORef (List a)
data List a  = Nil | Cons a (ListRef a)-- see what we're doing here ;)

fromList :: [a] -> IO (ListRef a)
fromList [] = do
    nilref <- newIORef Nil
    pure nilref
fromList (x:xs) = do
    ys <- fromList xs
    ref <- newIORef (Cons x ys)
    pure ref
    
length' :: ListRef a -> IO Int
length' l = do
    val <- readIORef l
    case val of
        Nil -> pure 0
        Cons x xs -> do
            n <- length' xs
            pure (n+1)

end :: ListRef a -> IO (ListRef a) 
-- returns a reference to the end of the list
end ref = do
    val <- readIORef ref
    case val of
        Nil -> pure ref
        Cons x ys -> do -- ys :: ListRef a = IORef (List a)
            end' <- end ys
            pure end'
            
endAndPrint :: Show a => ListRef a -> IO (ListRef a)
endAndPrint ref = do
    val <- readIORef ref
    case val of
        Nil -> pure ref
        Cons x ys -> do -- ys :: ListRef a = IORef (List a), x :: a
            end' <- endAndPrint ys
            print x
            putStr "\n"
            pure end'
                    

append :: ListRef a -> ListRef a -> IO ()
append ref1 ref2 = do
    end1 <- end ref1 -- val1 :: listRef a = IORef (List a)
    copy end1 ref2
    
example :: IO ()
example = do
    x <- fromList [ 0..14 ]
    y <- fromList [15..19]
    append x y
    n1 <- length' x
    print n1
    append x y
    e <- endAndPrint y -- will not terminate: the end of y points to the head of y
    print "Bye"
    
    
