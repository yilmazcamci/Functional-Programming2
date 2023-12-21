module PolyTypes where

justs :: [Maybe Char] -> [Char]
justs xs           = [ x | Just x <- xs, x /= ' ' ]

orderPairs :: Ord a => [(a,a)] -> [(a,a)]
orderPairs xs      = map (\(x,y)->(min x y, max x y)) xs

unmaybe :: Maybe (Maybe a) -> Maybe a
unmaybe (Just x)   = x
unmaybe Nothing    = Nothing

accumulate :: (d -> (a,d)) -> d -> [a]
accumulate f st = let (x,st') = f st 
                  in x : accumulate f st'
