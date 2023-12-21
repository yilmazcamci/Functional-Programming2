-- 1.
-- Peano axioms by introducing a new data type:

data Nat = Z | S Nat -- the natural numbers are defined as a set N with Z in N and if n in N then S n in N

instance Eq Nat where 
    Z == Z     = True
    Z == _     = False
    _ == Z     = True
    (S n) == (S m) = n == m

instance Ord Nat where 
    Z < Z   = False
    Z < _   = True
    _ < Z   = False
    (S n) < (S m) = n < m 
    n <= m = (n < m) || (n == m)

n2int :: N -> Int
n2int Z = 0
n2int (S n) = 1 + (n2int n)

instance Show N where
    show n = show (n2int n)

add :: Nat -> Nat -> Nat
add Z n = n
add (S n) m = add n (S m)

mult :: Nat -> Nat -> Nat
mult Z n = Z
mult (S n) m = add (mult n m) m

-- 2.
data SearchTree a = Nil | Node a (SearchTree a) (SearchTree a)
occurs :: Ord a => SearchTree a -> a -> Bool
occurs Nil = False
occurs (Node x left right) y = case compare x y of
  EQ -> True
  LT -> occurs right y
  GT -> occurs left  y
  
-- 3.
data Tree a = Leaf a | Node (Tree a) (Tree a)
balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left right) = abs $ nrLeaves left - nrLeaves right <= 1 where
    nrLeaves (Leaf a) = 1
    nrLeaves (Node left right) = nrLeaves left + nrLeaves right
    
-- 4. 
balance :: [a] -> Tree a
balance [x] = Leaf x
balance  l  = Node (balance $take half l) (balance $drop half l) where
    half = length l `div` 2
    

-- 5.
data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val i) = f i
folde f g (Add e1 e2) = g (folde f g e1) (fole f g e2)

-- 6.
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- 7.
instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True -- do we want null == null? SQL says no
    Nothing == _       = False
    _       == Nothing = False
    Just x  == Just y  = x == y
    
instance Eq a =>> Eq [a] where
    []      == []      = True -- if toMaybe, we would like l1 == l2 iff. toMaybe l1 == toMaybe l2, so that is a motivation for defining Nothing == Nothing = True
    []      == _       = False
    _       == []      = False
    (x:xs)  == (y:yx)  = x == y && xs == ys
    
-- 8.
    
-- The following code extends some of the code already written in tautology.hs
-- by Graham Hutton:
-- It adds constructors 'Or' and 'Equiv' for Prop, and extends the tautology
-- checker to support the use of these constructors.

-- Propositions

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or  Prop Prop
          | Equiv Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- Substitutions

type Subst = Assoc Char Bool

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

-- Tautology checker

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q
eval s (Or p q) = eval s p || eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)   = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
    
    
-- 9.

-- The following code extends some of the code already written in machine.hs
-- by Graham Hutton:
-- It adds constructors 'Or' and 'Equiv' for Prop, and extends the tautology
-- checker to support the use of these constructors.

data Expr = Val Int | Bin Op Expr Expr 

type Cont = [Stmt]

data Stmt = EVAL Op Expr | DO Op Int
-- if you have many similarly-structured constructors, consider factoring
-- out the commonality as an enumerated type. For example, we have a 
-- binary operator expression that has an operator and two expressions.

data Op = Add | Mult

eval :: Expr -> Cont -> Int
eval (Val n)    c = exec c n
eval (Bin op x y)  c = eval x (EVAL op y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL op y : c) n = eval y (DO op n : c)
exec (DO Add n : c)  m = exec c (n+m)
exec (DO Mult n: c)  m = exec c (n*m)

value :: Expr -> Int
value e = eval e []
 
