{-
Pentagon numbers
Problem 44
Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2. The first ten pentagonal numbers are:

1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...

It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference, 70 − 22 = 48, is not pentagonal.

Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal and D = |Pk − Pj| is minimised; what is the value of D?
-}

import Data.List
import Math.NumberTheory.Powers.Squares

maxn = 10000

pairs :: [Int] -> [(Int,Int)]
pairs s = [(x,y) | (x:xt) <- tails s, y <- xt]

pent :: Int -> Int
pent n = n*(3*n-1) `div` 2

is_pent :: Int -> Bool
is_pent x = (isSquare p) && ((mod q 6) == 5)
  where
    p = (24*x) + 1
    q = integerSquareRoot p

-- check a pair
check :: (Int,Int) -> Bool
check (a,b) = and $ map (is_pent . abs) [a+b,a-b]

solve = 
  map (\(a,b) -> abs (a-b))
  $ filter check
  $ pairs
  $ map pent [1..maxn]


main = do
  print $ pairs [1,2,3,4,5]
  print $ take 10 $ map pent [1..maxn]
  print $ take 10 $ pairs $ map pent [1..maxn]
  print solve

{-
It would be useful to be able to generate the pairs incrementally
and in ascending order of D.
Runtime is about 4 seconds at maxn=10k, but never finishes for maxn=100k.
Tried using a set instead of "is_pent", but it was slower.
So I got lucky, but don't know how to make it better.
-}





