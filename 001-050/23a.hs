{-
Non-abundant sums
Problem 23
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
-}

import Data.Numbers.Primes
import Data.List
import qualified Data.Set as Set


limit = 28123
--limit = 100

divisors :: Int -> [Int]
divisors n = nub $ sort $ map product $ subsequences $ primeFactors n

proper_divisors :: Int -> [Int]
proper_divisors n = init $ divisors n

d :: Int -> Int
d n = sum $ proper_divisors n

is_abundant :: Int -> Bool
is_abundant n = (d n) > n

-- all abundant numbers
a = filter is_abundant [1..limit]

-- all sums of pairs of abundant numbers
b = [ x+y | x <- a, y <- a, ((x+y) <= limit) && (x <= y)]

c = Set.fromList b
e = Set.fromList [1..limit]
f = Set.difference e c
g = Set.toList f

main = do
  print $ take 10 a
  print $ sum g


