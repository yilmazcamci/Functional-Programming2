import Data.List

splitOnce :: Eq a => a -> [a] -> ([a], Maybe [a])
splitOnce x [] = ([],Nothing)
splitOnce x (y:ys) | x == y = ([], Just ys)
                   | otherwise = 
                        let (before, after) = splitOnce x ys
                        in (y:before, after)
                        
split :: Eq a => a -> [a] -> [[a]]
split x ys = 
    let (before, after) = splitOnce x ys
    in case after of
        Nothing  -> before:[]
        Just ys' -> before:(split x ys')
        
{- Mental note:

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

unfoldr step acc = case step acc of
    Nothing    -> []
    Just (x,y) -> x:(unfoldr step y)

Clearly, use b = Maybe [a]
And splitOnce x :: [a] -> ([a], Maybe [a]),
so we should lift the Maybe [a] through the tuple somehow.


-}
        
split' :: Eq a => a -> [a] -> [[a]]
split' x ys = unfoldr st ys where
    st zs = case splitOnce x zs of
        (out, Nothing) -> Nothing
        (out, Just more) -> Just (out, more)
              
              
