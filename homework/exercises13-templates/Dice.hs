module Dice where

import System.Random
import Control.Monad
import Control.Applicative
import Data.List
import Text.Read
import RandomState
import RandomGen
import LCG

data Expr = Lit Int | Dice Int 
          | Expr :+: Expr | Min Expr Expr
          | Expr :-: Expr | Max Expr Expr
          | Expr :/: Int
  deriving (Eq, Show)

infixl 6 :+: 
infixl 4 :/: 
infixl 6 :-: 

type DiceAction m = Int -> m Int

evalM :: (Monad m) => Expr -> DiceAction m -> m Int  -- final version
evalM (Lit i) _       = pure i

evalM (Dice i) act    = act i >>= pure

evalM (e1 :+: e2) act = pure (+) <*> evalM e1 act <*> evalM e2 act 

evalM (e1 :-: e2) act = pure (-) <*> evalM e1 act <*> evalM e2 act 

evalM (Min e1 e2) act = pure (min) <*> evalM e1 act <*> evalM e2 act

evalM (Max e1 e2) act = pure (max) <*> evalM e1 act <*> evalM e2 act

evalM (e1 :/: c) act = evalM e1 act >>= \ u ->
                       pure (u `div` c)

  


--evalRIO :: Expr -> IO Int
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= pure)   -- silent version
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
--  where report x = do { putStr "rolled a "; print x; pure x }

evalIO :: Expr -> IO Int
evalIO expr = evalM expr userDiceRoll where
  
userDiceRoll :: DiceAction IO
userDiceRoll i = do
  putStrLn ("Please enter a die roll within range [1, " ++ show i ++ "].") 
  putStr "> " 
  line <- getLine
  case readMaybe line of
    Just k -> if (k >= 1 && k <= i) then pure k else putStrLn "Your roll was not in the required range. Try again." >> userDiceRoll i
    Nothing -> putStrLn "That was not an integer." >> userDiceRoll i

evalND :: Expr -> [Int]
evalND expr = evalM expr allRolls where
  allRolls :: DiceAction []
  allRolls = \ x -> [1..x]

avg :: (Fractional a) => [Int] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

expectation :: (Fractional a) => Expr -> a
expectation e = avg (evalND e)

histogram = map (\ x -> (head x,length x)) . group . sort . evalND

evalR :: Expr -> RandomState Int
evalR expr = evalM expr (\k -> state (randomRange (1,k)))

observed :: (Fractional a) => Int -> Expr -> RandomState a
observed 0 _ = pure 0
observed n expr = do
  outcome <- evalR expr
  other <- observed (n-1) expr
  pure $ (fromIntegral outcome + other * fromIntegral (n-1)) / fromIntegral n
