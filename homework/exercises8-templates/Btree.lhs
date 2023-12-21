> module Btree where
>
> data Btree a = Tip a | Bin (Btree a) (Btree a)
>
> mapBtree :: (a -> b) -> Btree a -> Btree b
> mapBtree f (Tip a)     = Tip (f a)
> mapBtree f (Bin t1 t2) = Bin (mapBtree f t1) (mapBtree f t2)

> tips :: (Btree a) -> [a]
> tips (Tip x) = [x]
> tips (Bin as bs) = tips as ++ tips bs

To prove: map f (tips r) = tips (mapBtree f r) for all f,t

With structural induction on Btree:
Case r = Tip x {Base}:
  map f (tips t) 
  ---------------------------- expand t
  = map f (tips (Tip x)) 
  ---------------------------- def. of tips
  = map f [x] 
  ---------------------------- un-syntactic-sugar [x]
  = map f (x: []) 
  ---------------------------- def. of map
  = (f x): (map f []) 
  ---------------------------- def. of map
  = (f x):[] 
  ---------------------------- syntactic-sugar y:[]
  = [f x]
  
  and:
  tips (mapBtree f t) 
  ---------------------------- expand t
  = tips (mapBtree f (Tip x)) 
  ---------------------------- def. of mapBtree
  = tips (Tip (f x)) 
  ---------------------------- def. of tips
  = [f x]
  
  so
  map f (tips r) = tips (mapBtree f r), for r = Tip x, for all x
  
Case r = Bin t s {Inductive case}
  given that it holds for smaller trees, i.e. t and s (induction hypothesis)
    suppose we already have the result for all smaller trees, then we want to prove it for
    Bin t s where t and s are smaller trees for which the result already holds.
    
  map f (tips (Bin t s)) 
  = map f ((tips t) ++ (tips s)) 
  ----------------------------  use that f distributes over ++ (proved in exercise 8.3.2)
  = map f (tips t) ++ map f (tips s)
  ---------------------------- use induction hypothesis on the smaller trees t and s
  = tips (mapBtree f t) ++ tips (mapBtree f s)

    And now, use the definition of mapBtree:
    
  mapBtree f (Bin t s) 
  ---------------------------- def. of mapBtree (recursive)
  = Bin (mapBtree f t) (mapBtree f s)
  
    so that 
    
  tips (mapBtree f (Bin t s)) 
  ---------------------------- 
  = tips (Bin (mapBtree f t) (mapBtree f s)) 
  ---------------------------- by definition of tips
  = tips (mapBtree f t) ++ tips (mapBtree f s)

    So the equality 
    map f (tips (Bin t s)) = tips (mapBtree f (Bin t s))
    holds for all t, s for which the induction hypothesis holds,
    proving the inductive step.

    

