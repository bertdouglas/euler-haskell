---------------------------------------------------------
-- Largest prime factor
-- Problem 3
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- 
-- What is the largest prime factor of the number 600851475143 ?
----------------------------------------------------------

-- To build:  ghc --make foo.hs
-- To run:    ./foo
-- Output:    6857

number = 600851475143

factors' :: Integer -> Integer -> [Integer] -> [Integer]
factors' i n fs =
  let
    found = 0 == (n `mod` i)
    stop = (i*i) > n
  in
    case (found,stop) of
      (True, False) -> factors' 2 (n `div` i) (i:fs)
      (False,False) -> factors' (i+1) n fs
      (_     ,True) -> n:fs

factors n = factors' 2 n []

answer = head $ factors number

main = print answer

{-
Exhaustive search.  Trial division by increasing values. 
Remove each factor by division as it is found.  Repeat.
Obviously the small factor will be found first, and the smallest factor
will be prime.  At the end, the head of the list will contain the
largest prime factor.

This is fast enough.  It would be better to start with a list of
known primes
-}

