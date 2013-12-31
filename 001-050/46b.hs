{-
Goldbach's other conjecture
Problem 46
It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

9 = 7 + 2×12
15 = 7 + 2×22
21 = 3 + 2×32
25 = 7 + 2×32
27 = 19 + 2×22
33 = 31 + 2×12

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
-}

import Data.List
import Data.Numbers.Primes
import Math.NumberTheory.Powers.Squares

pri = primes

candidates :: Int -> [Int]
candidates n = 
  zipWith (-) (repeat n)
  $ takeWhile (<=n) pri

is_twice_square :: Int -> Bool
is_twice_square n = isSquare (n `div` 2)

solve =
  (+) 2
  $ head . head
  $ take 1
  $ filter (not . any is_twice_square)
  $ map candidates [3,5..]

main = do
  print solve

{-
This version is much faster at 12 milliseconds. It guarrantees correct result, rather than relying on luck.  It checks fewer cases, checks them in order, stops when it finds the first result, and does not build huge lists, but rather consumes them immediately.  There is no magic "maxn".  Note there is no direct recursion, everything is done with built in higher order functions, so it is actually simpler to code.

There is a choice to generate candidates based on the primes or on squares. It is faster to use squares for candidates, because they increase faster than primes, and so there are fewer.

Then the check must be for square or for primality.  I believe the square check is much faster than a primality check, and this dominates.
-}

