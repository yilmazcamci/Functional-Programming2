module ListIORef where

import Data.IORef

{-

data IORef a

newIORef :: a -> IO (IORef a)\

readIORef :: IORef a -> IO a

writeIORef :: IORef a -> a -> IO ()

modifyIORef :: IORef a -> (a -> a) -> IO () 

-}

ListIORef = IORef ListRef
ListRef   = Nil | Cons LisIORef -- see what we're doing here ;)


