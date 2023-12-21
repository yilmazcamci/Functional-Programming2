module AST where

import Result
-- this template uses prefix constructors; feel free to use ASTInfix.hs (which uses infix ones) if you prefer
-- (if you really liked your own solution to Exercise 4.7, you can use that as well)

type Identifier = String

data Expr = Lit Integer | Var Identifier | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving (Show)

safediv :: (Fractional n, Eq n) => n -> n -> Result n
safediv _ 0 = Error ["division by 0"]
safediv x y = Okay $ x/y

find :: Identifier -> [(Identifier,v)] -> Result v
find key [] = Error ["unknown variable: " ++ key]
find key ((key',val):assocs) = if key == key' then Okay val else find key assocs

eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Result a 
eval (Lit k) _ = Okay (fromInteger k) 
eval (Add x y) vars = (+) <$> (eval x vars) <*> (eval y vars)
                       
eval (Sub x y) vars = (-) <$> (eval x vars) <*> (eval y vars)

eval (Mul x y) vars = (*) <$> (eval x vars) <*> (eval y vars)

eval (Div x y) vars = eval x vars >>= \ u ->
                      eval y vars >>= \ v ->
                      u `safediv` v

eval (Var name) vars = find name vars
