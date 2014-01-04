{-
Pandigital prime
Problem 41
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?
-}

import Data.List
import Data.Numbers.Primes

djoin :: [Int] -> Int
djoin s = djoin' s 0
  where
    djoin' s n
      | s == []   = n
      | otherwise = djoin' t ((n*10)+h)
        where
          h = head s
          t = tail s

solve =
  take 1
  $ filter isPrime
  $ reverse
  $ sort
  $ map djoin
  $ concat
  $ map permutations 
  $ map (\x -> [1..x]) [1..9]

main = do
  print solve

{-
First I tried searching through all primes for pandigitals, but there are too many.  It is much faster to generate pandigitals and check for primality.
-}

