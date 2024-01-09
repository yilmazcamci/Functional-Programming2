-----------------------------------------------------
To prove: map (f . g) xs = map f (map g xs)
By induction on xs.

Case 1: xs = []

    map (f . g) []
    --------------{using eq (3) for map}
  = []
    -- {using eq (3) for map}
  = map f []
          -- {using eq (3) for map}
  = map f (map g [])

Case 2: xs = (a:as)
IH: map (f . g) as = map f (map g as), for all f and g

    map (f . g) (a:as)
    ------------------{using eq (4) for map}
  = (f . g) a : map (f . g) as
                --------------{using IH}
  = (f . g) a : map f (map g as)
    -------{using eq (0)}
  = f (g a) : map f (map g as)
    --------------------------{using eq (4) for f, g a : map g as}
  = map f (g a : map g as)
           --------------{using eq (5) for g, a:as}
  = map f (map g (a:as))


-----------------------------------------------------
To prove: map f (as ++ bs) = (map f as) ++ (map f bs)
By induction on as.

Case 1: as = []

    map f ([] ++ bs)
           --------{eq (1), ys = bs}
  = map f bs
    --------{eq 1, ys = map f bs
  = [] ++ map f bs
    --{eq (3)}
  = map f [] ++ map f bs

Case 2: as = x:xs
IH: map f (xs ++ bs) = (map f xs) ++ (map f bs), for all f and bs

    map f (x:xs ++ bs) 
           ----------{eq (2)}
  = map f (x:(xs ++ bs))
    --------------------{eq (4) for f, x:(xs ++ bs)}
  = f x : map (xs ++ bs)
          --------------{IH}
  = f x : ((map f xs) ++ (map f bs))
    --------------------------------{eq (2) for f x, (map f xs), (map f bs)
  = (f x : (map f xs)) ++ (map f bs)
    ------------------{eq (4) for f, x:xs}
  = (map f (x:xs)) ++ (map f bs)
  
-----------------------------------------------------
To prove: concat (map (map f) xs) = map f (concat xs)
By induction on xs.

Case 1: xs = []

    concat (map (map f) []) 
    
  = concat []
  
  = []
  
  = map f []
  
  = map f (concat [])

Case 2: xs = (a:as)
IH: concat (map (map f) as) = map f (concat as) for all f

    concat (map (map f) (a:as)) 
    
  = concat (map f a : map (map f) as)
  
  = map f a ++ concat (map (map f) as)
  
  = map f a ++ map f (concat as)
    ----------------------------{using lemma 2}
  = map f (a ++ concat as)
  
  = map f (concat (a:as))
  
  
