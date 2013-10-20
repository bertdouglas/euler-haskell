{-
Quadratic primes

Problem 27
Euler discovered the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n² + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |−4| = 4
Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
-}

import Data.Numbers.Primes
import Data.List

longer :: [a] -> [a] -> [a]
longer a b = 
  if (length a) >= (length b)
    then a
    else b

quad (a,b,n) = n^2 + a*n + b

crange = [-1000..1000]
step1 :: [[(Int,Int,Int)]]
step1  = [[ (a,b,n) | n <- [0..] ] | a <- crange, b <- crange ]
step2  = map (takeWhile (isPrime . quad)) step1
step3  = filter (/=[]) step2
step4  = foldl1 longer step3
step5  = last step4

main = do
  --print crange
  --print $ take 10 step1
  --print step3
  print step5
  print (a*b) where (a,b,n) = step5

{-
Brute force exhaustive search.  Takes about 0.5 second.
-}

