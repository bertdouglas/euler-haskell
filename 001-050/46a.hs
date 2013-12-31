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

maxn = 1000

cartProd xs ys = [(x,y) | x <- xs, y <- ys]

pri = take maxn primes
sq2 = map (\x -> 2*(x^2)) [0..maxn]

check :: [Int] -> (Int,Int)
check s
  | (length s) < 2   = (0,0)
  | 2 /= (b-a)       = (a,b)
  | otherwise        = check (tail s)
    where
      a = head s
      b = head $ tail s

solve =
  check
  $ filter odd
  $ map head
  $ group
  $ sort
  $ map (\(a,b) -> a+b)
  $ cartProd pri sq2

main = do
  print solve

{-
Most of the runtime is in the "check" function, and I don't understand
why.
-}

