module Uniq where

uniq :: (Eq a) => [a] -> [a]

uniq [] = []
uniq [a] = [a]
uniq (a:as) = if a == head as then uniq as else a:(uniq as)