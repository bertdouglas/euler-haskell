---------------------------------------------------------
-- Multiples of 3 and 5
-- Problem 1
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, 
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- 
-- Find the sum of all the multiples of 3 or 5 below 1000.
----------------------------------------------------------

-- To build:  ghc --make foo.hs
-- To run:    ./foo
-- Output:    233168

import Data.List

limit = 1000

multiples_of_a_below_n a n = [1*a,2*a..n-1]

m3 = multiples_of_a_below_n 3 limit
m5 = multiples_of_a_below_n 5 limit

answer = sum $ union m3 m5

main = print answer

{-
This code is basically the same as the problem statement.  It is fast
enough, but i fear it would not scale well to large n.
-}
