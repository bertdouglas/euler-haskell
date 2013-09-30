--------------------------------------------------------------------------------
-- Highly divisible triangular number
-- Problem 12
-- The sequence of triangle numbers is generated by adding the natural numbers. 
-- So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. 
-- The first ten terms would be:

-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

-- Let us list the factors of the first seven triangle numbers:

--  1: 1
--  3: 1,3
--  6: 1,2,3,6
-- 10: 1,2,5,10
-- 15: 1,3,5,15
-- 21: 1,3,7,21
-- 28: 1,2,4,7,14,28
   
-- We can see that 28 is the first triangle number to have over five divisors.

-- What is the value of the first triangle number to have over five hundred divisors?
--------------------------------------------------------------------------------

-- some further examples also showing prime factors.

-- ========    ===========     ===========================================
-- triangle       prime        divisors
--  number       factors       
-- ========    ===========     ===========================================
--      3      [3]             [1,3]
--      6      [2,3]           [1,2,3,6]
--     28      [2,2,7]         [1,2,4,7,14,28]
--     36      [2,2,3,3]       [1,2,3,4,6,9,12,18,36]
--    120      [2,2,2,3,5]     [1,2,3,4,5,6,8,10,12,15,20,24,30,40,60,120]
-- ========    ===========     ===========================================

import Data.Numbers.Primes
import Data.List

-- infnite list of triangle numbers [1,3,6,10..]
triangles :: Integral a => [a]
triangles = scanl1 (+) [1..]

-- take a list of prime factors and return a list of divisors
divisors factors =
    nub $ sort $ map product $ subsequences factors


solve limit = solve' triangles
  where
    solve' ts
      | n >= limit = (limit,n,t,p,d)
      | otherwise = solve' (tail ts)
      where
        t = head ts
        p = primeFactors t
        d = divisors p
        n = length d

main = do
  print $ take 20 triangles

  print $ solve 2
  print $ solve 3
  print $ solve 4
  print $ solve 5
  print $ solve 501

