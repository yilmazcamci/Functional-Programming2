> module Value where

In an interpreter it is often convenient to work with a single type for all values. If we want our
interpreter to support different kinds of numbers, lists and strings, we could use the following
type to represent values:

\begin{code}

data Value = VInt Int
    | VDouble Double
    | VString String
    | VList [Value]
    deriving (Eq)

\end{code}

To convert from Haskell values to Values we can introduce a typeclass

\begin{code}

class ToValue a where
    toValue :: a -> Value

\end{code}

(a)(1) Give a suitable instance of ToValue for type Int.

\begin{code}

instance ToValue Int where
    toValue = VInt

\end{code}


(b)(1) Give a suitable instance of ToValue for lists.

\begin{code}

instance ToValue a => ToValue [a] where
    toValue xs = VList (map toValue xs)

\end{code}

(c)(1) Tuples can be encoded as values using the VList constructor. For example (1,1.5)
would be encoded as VList [VInt 1, VDouble 1.5]. Give an instance of ToValue
for type (a,b) that uses this encoding.


\begin{code}

instance (ToValue a, ToValue b) => ToValue (a,b) where
    toValue (x,y) = VList [toValue x, toValue y]

\end{code}

(d)(1) Define a type class FromValue with a method fromValue that converts from Values
back to haskell values of a certain type. This conversion can fail, use Maybe to represent
this possible failure.

\begin{code}

class FromValue a where
    fromValue :: Value -> Maybe a

\end{code}

(e)(1) Give a suitable instance of FromValue for type Int.

\begin{code}

instance FromValue Int where
    fromValue (VInt i) = Just i
    fromValue _        = Nothing

\end{code}

(f)(1) Give a suitable instance of FromValue for lists.

\begin{code}

instance (FromValue a) => FromValue [a] where
    fromValue (VList xs) = mapM fromValue xs
    fromValue _          = Nothing

\end{code}

This enables one to have lists that hold different "wrapped" types simultaneously,
while gracefully letting the interpretation into a monolithic type list fail. For example:


\begin{code}

ex1 = fromValue (VList [VInt 4, VInt 0]) :: Maybe [Int]
-- Just [4,0]

ex2 = fromValue (VList [VInt 4, VInt 0]) :: Maybe Int
-- Nothing

ex3 = fromValue (VList [VInt 4, VDouble 5.0]) :: Maybe [Int]
-- Nothing


\end{code}

