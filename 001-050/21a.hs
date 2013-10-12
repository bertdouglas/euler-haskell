--------------------------------------------------------------------------------
-- Amicable numbers
-- Problem 21
-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n 
-- which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair 
-- and each of a and b are called amicable numbers.

-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 
-- 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 
-- 1, 2, 4, 71 and 142; so d(284) = 220.

-- Evaluate the sum of all the amicable numbers under 10000.
--------------------------------------------------------------------------------

import Data.Numbers.Primes
import Data.List

limit = 10000

divisors :: Integer -> [Integer]
divisors n = nub $ sort $ map product $ subsequences $ primeFactors n

proper_divisors :: Integer -> [Integer]
proper_divisors n = init $ divisors n

d :: Integer -> Integer
d n = sum $ proper_divisors n

is_amicable :: Integer -> Bool
is_amicable n = (n == (d a)) && (a /= n)
  where
    a = d n

amicables n = filter is_amicable [1..n]
a = amicables (limit-1)

main = do
  print $ proper_divisors 220
  print $ proper_divisors 284
  print $ d 220
  print $ d 284
  print $ is_amicable 220
  print $ is_amicable 284
  print $ a
  print $ map d a
  print $ sum a


