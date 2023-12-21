module Database where

type Person = (Name, Age, FavouriteCourse)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

elena, peter, pol :: Person
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming")
pol    =  ("Pol",    36,  "Object Oriented Programming")
matthijs = ("Matthijs", 19, "Complexity")
allart = ("Allart", 19, "Algorithms")
frits = ("Frits", 80, "Graph Mining")

students :: [Person]
students = [elena, peter, pol, matthijs, allart, frits]

age :: Person -> Age
age (_, n, _)  =  n

name :: Person -> Name
name (n, _, _) = n

favouriteCourse :: Person -> FavouriteCourse
favouriteCourse (_, _, n) = n

showPerson :: Person -> String
showPerson (n, a, fc) = n ++ ", " ++ show a ++ ", " ++ fc

twins :: Person -> Person -> Bool
twins (_, n, _) (_, m, _) = n == m

increaseAge :: Person -> Person
increaseAge (a, b, c) = (a, b+1, c)

-- first develop the expressions in GHCi, then replace the TODO's below with them
query1 :: [Person]
query1 = map increaseAge students

query2 :: [Person]
query2 = map (\(n, a, fc) -> ("dr. " ++ n, a, fc)) students

query3 :: [Person]
query3 = filter ((== "Frits") . name) students

query4 :: [Person]
query4 = filter (\st -> 0 <= age st && age st < 30) students 

query5 :: Age
query5 = sum_age `div` nr_students
    where   sum_age = fromIntegral (sum (map age students))
            nr_students = fromIntegral (length students)

query6 :: [Person]
query6 = map promoteIfLikesFP students
    where promoteIfLikesFP (n, a, fc) = if fc == "Functional Programming" then ("dr. " ++ n, a, fc) else (n, a, fc)

-- if you have removed all TODO's, remove these lines
_TODO :: String -> a
_TODO msg = error ("TODO: " ++ msg)
