module LinkedList where

import Data.IORef

type ListRef elem = IORef (List elem)

data List elem = Nil | Cons elem (ListRef elem)

nil :: IO (ListRef elem)
nil = error "nil: not yet implemented"

cons :: elem -> ListRef elem -> IO (ListRef elem)
cons = error "cons: not yet implemented"

fromList :: [elem] -> IO (ListRef elem)
fromList = error "fromList: not yet implemented"

toList :: ListRef elem -> IO [elem]
toList = error "toList: not yet implemented"

foreach :: ListRef a -> (a -> IO b) -> IO (ListRef b)
foreach = error "foreach: not yet implemented"

--inplace_foreach :: ???
