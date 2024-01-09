> module ReverseCat where
> import Prelude hiding (reverse)
>
> reverse :: [a] -> [a]
> reverse [] = []
> reverse (x:xs) = reverse xs ++ [x]
>
> reverse' :: [a] -> [a]
> reverse' xs = reverseCat xs []
>
> reverseCat :: [a] -> [a] -> [a]
> reverseCat [] ys = ys
> reverseCat (x:xs) ys = reverseCat xs (x:ys)

---------------------------------------------
To prove: reverseCat xs ys = reverse xs ++ ys
By induction on xs.

Case 1: xs = []

    reverseCat [] ys
    ---------------- definition of reverseCat
  = ys
    --               definition of ++
  = [] ++ ys         
    --               definition of reverse
  = reverse [] ++ ys

Case 2: xs = (a:as)
IH: reverseCat as bs = reverse as ++ bs, for all bs.

    reverseCat (a:as) ys
    ---------------------definition of reverseCat
  = reverseCat as (a:ys)
    -------------------- IH with bs = (a:ys)
  = reverse as ++ (a:ys)
                     -- definition of ++
  = reverse as ++ (a:([] ++ ys))
                   ------------ definition of ++
  = reverse as ++ ((a:[]) ++ ys)
                    ---- definition of :
  = reverse as ++ ([a] ++ ys)
            ----------------- associativity of ++
  = (reverse as ++ [a]) ++ ys
     ---------- definition of reverse
  = reverse (a:as) ++ ys


-----------------------------------------------------
To prove: reverse xs = reverse' xs

Case 1: xs = []

    reverse' []
  = reverseCat [] []
  = []
  = reverse []
  
  
Case 2: xs = (a:as)
IH: reverse as = reverse' as

    reverse (a:as)
    ---------------- definition of reverseCat
  = reverse as ++ [a]
    ---------------- definition of reverseCat
  = reverseCat as [a] 
  = reverseCat as (a:[])
  = reverseCat (a:as) []
  = reverse' (a:as)
