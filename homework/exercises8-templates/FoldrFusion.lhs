> module FoldrFusion where
>
> import Prelude hiding (map)

The foldr fusion law that

for all
  f :: b -> c
  g :: a -> b -> b
  h :: a -> c -> c
  z :: b
IF, for all x,y:
  f (g x y) = h x (f y)
THEN
  f . foldr g z = foldr h (f z)

----------------------------------------------

We can define map in terms of foldr:

> map :: (a -> b) -> [a] -> [b]
> map f = foldr (step f) []
>   where step f x xs = (f x) : xs

----------------------------------------------

To prove:  foldr p e . map q = foldr (p . q) e

We can apply the fusion law using
  f ==> foldr p e
  g ==> step q
  h ==> (p . q)
  z ==> []

As follows:

  foldr p e . map q
= foldr p e . foldr (step q) []     Using the foldr definition of map
= foldr (p . q) (foldr p e [])      Using the foldr fusion law
= foldr (p . q) e                   Using the definition of foldr


Since the "THEN" part of the fusion law can only be applied if the "IF" part is true,
we need to show that for all x, y:

   foldr p e (step q x y) = (p . q) x (foldr p e y)

Which is the case since:
foldr p e (step q x y) 
= foldr p e (q x:y)           Per definition of step
= p q x (foldr p e y)         Per definition of foldr
= (p . q) x (foldr p e y)     Per definition of .

----------------------------------------------
To prove:  map (f . g) = map f . map g
We have
map (f . g) 
= foldr (step (f . g)) []     Using the foldr definition of map
= foldr (step f . g) []       Using step (f . g) = step f . g
= foldr (step f) [] . map q   Using the foldr-map fusion law
= map f . map q               Using the foldr definition of map

It can be helpful to also prove that:  step (f . g) = step f . g:
We have for all x and y:
step (f . g) x y 
= (f . g x) : y               Per definition of step
= step f (g x) y              Per definition of step
= step f . g x y
Since this holds for all x and y, we conclude that step (f . g) = step f . g

----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

