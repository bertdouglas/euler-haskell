--------------------------------------------------------------------------------
-- 10001st prime
-- Problem 7
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
-- we can see that the 6th prime is 13.

-- What is the 10 001st prime number?
--------------------------------------------------------------------------------

import Data.Numbers.Primes

n = 10001

main = print $ last $ take n primes

{-
I know this is kind of lame.
Will make another version with my own prime number code.
-}
