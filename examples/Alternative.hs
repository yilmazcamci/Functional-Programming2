module Alternative hiding Control.Applicative.Alternative where

class Applicative (t :: * -> *) where
    pure :: a -> t a
    (<*>) :: f (a -> b) -> f a -> f b
    (<$>) :: a -> t a
    (<$>) = pure

class Applicative t => Alternative (t :: * -> * ) where
    emtpy :: t b
    (<|>) :: t b -> t b -> t b
    
    
    
    some :: b -> t [b]
    some v = (:) <$> v <*> many v

    many :: b -> t [b]
    many v = some v <|> pure [] 
    
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just f <*> Just x = Just (f x)
    
     
instance Alternative Maybe where
    empty = Nothing
    
    Just x <|> _   = Just x
    Nothing <|> a2 = a2
    

    
