module ASTInfix where

-- this template uses infix constructors; feel free to use AST.hs (which uses prefix ones) if you prefer
-- (if you really liked your own solution to Exercise 4.7, you can use that as well)

type Identifier = String

data Expr = Lit Integer | Var Identifier | Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr
  deriving (Show)

infixl 6 :+: 
infixl 6 :-: 
infixl 7 :/:
infixl 7 :*:

eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Maybe a 
eval (Lit k) _ = Just (fromInteger k) 
eval (x :+: y) vars = case (eval x vars, eval y vars) of 
                     (Just x', Just y') -> Just (x'+y')
                     _ -> Nothing
                       
eval (x :-: y) vars = case (eval x vars, eval y vars) of 
                     (Just x', Just y') -> Just (x'-y')
                     _ -> Nothing

eval (x :*: y) vars = case (eval x vars, eval y vars) of
                     (Just x', Just y') -> Just (x'*y') 
                     _ -> Nothing 

eval (x :/: y) vars = case (eval x vars, eval y vars) of 
                     (Just  _, Just 0)  -> Nothing
                     (Just x', Just y') -> Just (x'/y') 
                     _ -> Nothing

eval (Var name) vars = error "FIXME: variable support"
