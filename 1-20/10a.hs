--------------------------------------------------------------------------------
-- Summation of primes
-- Problem 10
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

-- Find the sum of all the primes below two million.
--------------------------------------------------------------------------------

import Data.Numbers.Primes

limit = 2000000

solve ps s = 
  let 
    p = head ps
  in
    if p < limit
      then solve (tail ps) (s+p)
      else s

answer = solve primes 0

main = print answer
