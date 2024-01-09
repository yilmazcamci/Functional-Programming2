(3) The functionpyramid :: [a] → [a] builds from the given list a pyramid, where element
𝑎𝑖 from the input list therefore occurs (𝑖 + 1) times in the result list. So, suppose the input
list is 𝑎0, 𝑎1, 𝑎2, ... then the result is 𝑎0, 𝑎1, 𝑎1, 𝑎2, 𝑎2, 𝑎2, .... Examples:

pyramid [1,2,3] = [1,2,2,3,3,3]
pyramid "marc" = "maarrrcccc"
pyramid [1,3..10] = [1,3,3,5,5,5,7,7,7,7,9,9,9,9,9]
pyramid [6,6,6] = [6,6,6,6,6,6]


Define pyramid using basic/library functions and/or list comprehensions, but not recur-
sion.

\begin{code}

pyramid :: [a] -> [a]
pyramid xs = concat (map (\(x,n) -> replicate n x) (zip xs [1..]))

\end{code}


(b)(3) In this part, lists are used to represent sets. The powerset of set 𝑆 is a set containing all
subsets of 𝑆. Write a (recursive) function powerset :: [a] → [[a]] that returns a set
containing all subsets of a given set. Examples:

powerset [1,2] = [[1,2],[2],[1],[]]
powerset "arc" = ["arc","rc","ac","c","ar","r","a",""]
powerset [] = [[]]

You may use recursion as well as functions from the standard prelude (see cheat sheet)
of Haskell. The order of the elements in the result list may deviate from the examples.
Furthermore, you do not have to consider the situation where the input list contains
duplicate elements (which in principle cannot be the case in a set).

\begin{code}

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

\end{code}
