module LiftM where

-- implement without 'fmap', '<$>' or '<*>'
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f mx = fmap f x
