{-
Distinct primes factors
Problem 47
The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?
-}


import Data.Numbers.Primes
import Data.List

solve n = solve' [1..]
  where
    solve' ns
      | distinct && n_factors = head ns
      | otherwise             = solve' (tail ns)
      where
        nns = take n ns
        fs = map (group . primeFactors) nns
        factors = foldl1 (++) fs
        distinct = factors == (nub factors)
        n_factors = and $ map (\x -> n == (length x)) fs

main = do
  print $ solve 2
  print $ solve 3
  print $ solve 4
