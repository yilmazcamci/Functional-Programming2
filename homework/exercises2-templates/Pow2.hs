module Pow2 where

pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 0 = 1
pow2 n | n > 0 = 2 * pow2 (n - 1)
       | n < 0 = 0 -- this makes no sense but we don't want the compiler to keep running 
                   -- and since (Num a), not (Fractional a), we don't have a support for fractions 1/2


-- 3. What is the maximum 𝑛 for which 2𝑛 can still be represented by the types Integer, Int, Float, and Double?

-- Answer:
-- for Int: 62, since MAX_INT = 2^{63} - 1
-- for Integer: no maximum (until you get a segmentation fault from you OS), because Integer is arbitrary-precision (the representation just expands when the value does not fit anymore)
-- for Float: 127. So apparently the exponent of the Float is lg 128 = 7 bits long, which leaves 32 - 7 = 25 for the mantissa?
-- for Double: 1023. So apparently Double has an exponent of lg 1024 = 10 bits, leaving 64 - 10 = 54 for mantissa.

