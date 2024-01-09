This question is about type classes. The class Serializable represents types whose ele-
ments can be converted to (encoded as) ByteStrings:

\begin{code}

class Serializable a where
    encode :: a -> ByteString
    
\end{code}

A ByteString is an abstract data type whose internal representation is not visible. We also
assume two operations to convert Ints and Chars into a ByteString and an operation to
combine two ByteStrings. The implementation of these operations is not important for
this assignment.

\begin{code} 

data ByteString

encodeChar :: Char -> ByteString
encodeInt :: Int -> ByteString
append :: ByteString -> ByteString -> ByteString

\end{code}

We ask you to provide instances of the class Serializable for some types. The encode
method must be implemented in such a way that any decoding (which we will not consider
further) must be able to return the original value. This excludes trivial/useless implementa-
tions, for example where you map all values to encodeInt 0.

(a)(1) Give a suitable instance of Serializable for type Int.

\begin{code}

instance Serializable Char where
    encode c = (encodeInt 0) `append` (encodeChar c)

\end{code}

(b)(1) Give a suitable instance of Serializable for type Bool.

\begin{code}

instance Serializable Int where
    encode i = (encodeInt 1) `append` (encodeInt i)

\end{code}

(c)(2) Give a suitable instance of Serializable for type Maybe.

\begin{code}

instance Serializable a => Serializable (Maybe a) where
    encode Nothing = encodeInt 2
    encode (Just v)= (encodeInt 3) `append` (encode v)

\end{code}

(d)(2) Give a suitable instance of Serializable for lists

\begin{code}

instance Serializable a => Serializable [a] where
    encode [] = encodeInt 4
    encode (x:xs) = (encodeInt 5) `append` ((encode x) `append` (encode xs))

\end{code}
