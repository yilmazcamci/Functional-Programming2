module SomeMany where

import Control.Monad(Functor(..))
import Control.Applicative
import Data.Char

-- Example of many and some:


-- Parser for sequences of i to a.
newtype Parser i a = P { runP :: [i] -> Maybe (a,String) }

-- runP (P p) s = p s

instance Functor (Parser i) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (P q) = P $ \s -> case q s of
        Just (y,ys) -> Just (f y,ys)
        Nothing     -> Nothing

instance Applicative (Parser i) where
    -- pure :: a -> f a
    pure x = P $ \s -> Just(x,s)
    -- (<*>) :: f (a -> b) -> f a -> f b
    P p <*> P q = P $ \s -> case p s of
        Nothing -> Nothing
        Just (f, ys)    -> case q ys of
            Nothing -> Nothing
            Just (x,xs) -> Just (x,xs)

letter :: Parser Char Char
letter = P p where 
    p (x:xs) | isAlpha x = Just (x,xs)
    p _ = Nothing
    
    
test1 = runP letter "123" == Nothing
test2 = runP letter "a123" == Just ('a',"123")

    -- some is one or more, 
    -- many is 0 or more results collected from performing 
    -- the same computation over and over by the familiar "maximal munch" rule (SO)
    -- In particular for stateful computations that reduce the argument passed
    -- in some way - especially in parsing

oneLetter =  (:) <$> letter <*> pure []

twoLetters = (:) <$> letter <*> ((:)<$>letter <*> pure [])

-- ... 
-- Ugly. But making Parser an instance of Alternative will help.

instance Alternative (Parser i) where
    empty = P $ \s -> Nothing
    P p <|> P q = P $ \s -> case p s of -- left-biased: in this case: if p succeeds, choose p
        Nothing     -> q s
        Just (x,ys) -> Just (x,ys)

manyLetters = (:) <$> (someLetters <|> pure []) -- if someLetters fails, pure [] = P $\s -> Just([],s) and (:) <$> will ensure concatenation with previous results.
someLetters = manyLetters <|> empty   -- if manyLetters fails, fail

  
manyLetters' = many letter -- equivalent to manyLetters
someLetters' = some letter -- equivalent to someLetters
  
