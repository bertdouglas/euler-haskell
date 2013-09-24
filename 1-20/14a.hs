--------------------------------------------------------------------------------
-- Longest Collatz sequence
-- Problem 14
-- The following iterative sequence is defined for the set of positive integers:

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the following sequence:

-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1) 
-- contains 10 terms. Although it has not been proved yet (Collatz Problem), 
-- it is thought that all starting numbers finish at 1.

-- Which starting number, under one million, produces the longest chain?

-- NOTE: Once the chain starts the terms are allowed to go above one million.
--------------------------------------------------------------------------------

limit = 10^6 -1

collatz n = collatz' n 0
  where
    collatz' n c
      | n==1   = (c+1)
      | even n = collatz' (n `div` 2) (c+1)
      | odd n  = collatz' (3*n +1)    (c+1)

solve i imax cmax
  | i==0 = (imax,cmax)
  | otherwise = 
    if c > cmax 
      then solve (i-1)  i     c
      else solve (i-1)  imax  cmax
    where
      c = collatz i
      

answer = solve limit 0 0

main = do
  print $ collatz 13
  print answer

{-
This seems to be a good place to apply memoization.  The paths towards 1
are shared.
Now taking about 40 seconds on my 7 year old laptop.
-}


