module Main where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import System.IO
import Control.Monad



{----------------------------- main part -------------------------------}
main :: IO ()
main = do
  code <- generateWord 4
  header (length code) 
  playGame code 12


{---------------------- functional parts -------------------------------}

data Colour = White | Silver | Green | Red | Orange | Pink | Yellow | Blue
  deriving (Show, Eq, Read, Ord, Bounded, Enum)

type CodeWord a = [a]

scoreAttempt :: (Ord a) => [a] -> [a] -> (Int,Int)
scoreAttempt code guess = (rightPos, wrongPos) where 
  rightPos = length (filter (\ pair -> fst pair == snd pair) (zip code guess))
  wrongPos = countRecursive (sort code) (sort guess) 0 - rightPos where
    countRecursive _ [] i = i
    countRecursive [] _ i = i
    countRecursive (x:xs) (y:ys) i 
      | x == y = countRecursive xs ys $! (i+1)
      | x < y  = countRecursive xs (y:ys) $! i
      | x > y  = countRecursive (x:xs) ys $! i

parse :: String -> (CodeWord Colour)
parse str = 
      let list_str = words str
      in [color | word <- list_str, color <- colors, toUpper (head word) == head (show color)]
      where colors = enumFrom(minBound :: Colour)


{---------------------- IO parts -------------------------------}


{---------------------- Random generators -------------------------------}
generateToken :: IO Colour
generateToken = do
  let low = fromEnum(minBound :: Colour)
  let high= fromEnum(maxBound :: Colour)
  i <- randomRIO(low, high) 
  pure ((toEnum i ):: Colour)
  
  
generateWord :: Int -> IO (CodeWord Colour)
generateWord l = replicateM l (generateToken :: IO Colour)  
  
{---------------------- User interaction -------------------------------}

header :: Int -> IO ()
header len = do
  putStr "I picked a random code word with "
  print (fromEnum(maxBound :: Colour) - fromEnum(minBound :: Colour))
  putStrLn " colours."
  putStr "It is "
  putStr (show len)
  putStrLn " pins long."
  putStr "Possible colours are "
  sequence_ (map (putStr . (\ x -> show x ++ " " )) (enumFrom (minBound :: Colour)))
  putStrLn " "

gameOver :: (Show a) => CodeWord a -> IO ()
gameOver w = putStrLn ("No more attempts left, game over. The code was " ++ show w)

win :: IO ()
win = putStrLn "Congratulations! You win!"
  
inCorrect :: Int -> Int -> IO ()
inCorrect correct_position wrong_position = do
  putStrLn "Incorrect"
  putStrLn (show correct_position ++ " correct pin(s) in the correct position")
  putStrLn (show wrong_position ++ " correct pin(s) in the wrong position")
  
tryToGuess :: Int -> IO ()
tryToGuess tries_left = do 
  putStr "Try to guess the secret code word, "
  putStr (show  tries_left)
  putStrLn " tries left."
  
yourGuess :: CodeWord Colour -> IO ()
yourGuess guess = do
  putStr "Your guess was: "
  sequence_ (map (putStr . (\ x -> show x ++ " " )) guess)
  putStrLn " "
  
playGame :: CodeWord Colour -> Int -> IO ()
playGame code tries = do
  tryToGuess tries
  if tries == 0 
    then do
      gameOver code
    else do
      putStrLn "Please enter your guess: "
      putStr "> "
      str <- getLine
      let parsed = parse str
      yourGuess parsed
      let t = scoreAttempt code parsed
      let corr = fst t
      let wrong = snd t
      let len = length code
      if corr == len
        then win
        else do 
            inCorrect corr wrong >> playGame code (tries - 1)
