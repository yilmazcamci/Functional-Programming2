module Replicate where

import Control.Monad hiding replicateM

-- this is the definition in the slides:
replicateM' :: (M.Monad m) => Int -> m a -> m [a]
replicateM' 0 _  = return []
replicateM' n mx = (:) <$> mx <*> replicateM' (n-1) mx

replicateM :: (M.Monad m) => Int -> m a -> m [a]
replicateM 0 _  = return []
replicateM n mx = do
    x <- mx
    xs <- replicateM (n-1) mx
    return (x:xs)
    
{-

e1 = replicateM 4 getLine   = IO which returns 4 getLine inputs
e2 = replicateM 4 Nothing   = Nothing
e3 = replicateM 4 (Just 37) = Just [37,37,37,37]
e4 = replicateM 4 [0,1]     = all 4-lists with elements from [0,1]


-}
