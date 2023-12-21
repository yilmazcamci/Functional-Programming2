module AST where

--start with either:
data Expr = Lit Integer | VarX | Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Div Expr Expr
    deriving (Show, Eq)


--or:
--  data Expr = Lit Integer | Expr :+: Expr | Expr :*: Expr | ...
--  infixl 6 :+:
--  infixl 7 :*:

eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
eval (Lit n) x = Just (fromIntegral n)
eval VarX x = Just x

eval (Div e1 e2) x = case (eval e1 x, eval e2 x) of
    (_,Just 0)  -> Nothing 
    (_,Nothing) -> Nothing
    (Nothing,_) -> Nothing
    (Just a,Just b)  -> Just (a / b)

eval (Add e1 e2) x = case (eval e1 x, eval e2 x) of
    (_,Nothing) -> Nothing
    (Nothing,_) -> Nothing
    (Just a,Just b)  -> Just (a + b)

eval (Mul e1 e2) x = case (eval e1 x, eval e2 x) of
    (_,Nothing) -> Nothing
    (Nothing,_) -> Nothing
    (Just a,Just b)  -> Just (a * b)
    
eval (Sub e1 e2) x = case (eval e1 x, eval e2 x) of
    (_,Nothing) -> Nothing
    (Nothing,_) -> Nothing
    (Just a,Just b)  -> Just (a - b)

derivative ::  Expr -> Expr
derivative (Lit _) = Lit 0
derivative VarX = Lit 1
derivative (Add e1 e2) = Add (derivative e1) (derivative e2)
derivative (Sub e1 e2) = Sub (derivative e1) (derivative e2)
derivative (Mul e1 e2) = Add (Mul (derivative e1) e2) (Mul e1 (derivative e2))
derivative (Div e1 e2) = Div (Sub (Mul e2 (derivative e1)) (Mul e1 (derivative e2))) (Mul e2 e2)