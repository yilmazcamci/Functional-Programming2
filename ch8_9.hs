    
-- 9.

-- The following code extends some of the code already written in machine.hs
-- by Graham Hutton:
-- It adds constructors 'Or' and 'Equiv' for Prop, and extends the tautology
-- checker to support the use of these constructors.

data Expr = Val Int | Bin Op Expr Expr 

type Cont = [Stmt]

data Stmt = EVAL Op Expr | DO Op Int

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
