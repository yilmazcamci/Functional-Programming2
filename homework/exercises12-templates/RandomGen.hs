module RandomGen where

import RandomState
import LCG

import System.Random
import Control.Monad

getRandomRange :: (Int,Int) -> RandomState Int
getRandomRange range = state $ \ seed -> randomRange range seed
  

--multiEval :: [RandomState a] -> RandomState [a]
--multiEval xs = do ..

roll_2d6 :: RandomState Int
roll_2d6 = do
  a <- getRandomRange (1,6)
  b <- getRandomRange (1,6)
  pure (a+b)

runRandomStateIO :: RandomState a -> IO a
runRandomStateIO action = do
  seed <- randomRIO (0 :: Int,1000 :: Int) 
  pure $ evalState action $ mkSeed seed

--these definitions can be used to test your function a bit more thoroughly
runRandomNumbers :: (Int,Int) -> Int -> Seed -> [Int]
runRandomNumbers range n seed = result
  where (result, _) = runState (replicateM n (getRandomRange range)) seed

testme :: [Int]
testme = runRandomNumbers (0,999) 100 (mkSeed 42)

{-
12.4.3
- What is the type of this expression? 
RandomState [Int]

- What does it compute? 
It "computes" a list of 3 entries, where each entry is the
result of two d6 dice added together (although, to be more
correct, it maps a state to a pair of such a list and a new 
state.

- How can you obtain its result?
Using evalState: this will evaluate inside the state monad,
and will ignore the state afterwards, giving as result only
the list. Note that there is only a "result" once we give
the RandomState [Int], which is really a wrapper around a
function GlobalState -> ([Int],GlobalState), a GlobalState.

So let our current state be currState
Then, the line of code to "get the result" (given our current
GlobalState) is:

result = evalState (sequence [roll_2d6, roll_2d6, roll_2d6]) currState

For example, running 

evalState (sequence [roll_2d6, roll_2d6, roll_2d6]) (mkSeed 2000)

will show

[4,7,2]

in ghci.


-}

class MonadRandom mr where
  getRandomR :: (Int,Int) -> mr Int
  
instance MonadRandom IO where
  getRandomR = randomRIO
  
instance MonadRandom RandomState where
  getRandomR = getRandomRange
