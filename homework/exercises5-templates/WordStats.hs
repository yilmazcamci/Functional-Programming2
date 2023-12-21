import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as M
--  ^^comment out this line if it causes compile errors (we need the `containers` library)

wordFrequency :: String -> [(String,Int)]
wordFrequency  = map (\x->(head x,length x)) . group . sort . words

mostFrequentOfLength :: Int -> String -> [String]
mostFrequentOfLength m = map fst . sortBy (flip (compare `on` snd)) . filter (\x -> length (fst x) >= m) . wordFrequency

wordLengthFrequency :: String -> [(Int,Int)]
wordLengthFrequency = map (\x -> (head x, length x)) . group . sort . map (\x -> length x) . words


anagrams :: String -> [[String]]
anagrams = filter (\x -> length x > 1) . map nub . map (map snd) . groupBy (\x y -> fst x == fst y) . sortBy (compare `on` fst) . map (\x -> (sort x, x)) . words

{- this 'main' function is only here for the compiled, stand-alone version
 - calling it from within GHCi is problematic since GHCi itself needs stdin!
 - to compile, run:
 -
 -     ghc -O WordStats
 -
 - (The -O flag turns on optimizations)
 -}
main :: IO ()
main = onStdin $ wordFrequency  -- change this to run a different function from the commandline
  where onStdin f = getContents >>= (mapM_ print . f . filter (\x->isAlphaNum x || isSpace x))


-- Use the story (Alice in Wonderland) included in the submission!!
