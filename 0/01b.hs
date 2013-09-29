---------------------------------------------------------
-- Multiples of 3 and 5
-- Problem 1
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, 
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- 
-- Find the sum of all the multiples of 3 or 5 below 1000.
----------------------------------------------------------

-- To build:  ghc --make foo.hs
-- To run:    ./foo

limit = 1000

solve i s =
  let
    m3 = 0 == (i `mod` 3)
    m5 = 0 == (i `mod` 5)
    i1 = i + 1
    s1 = if m3 || m5
      then s + i
      else s
  in
    if i < limit
      then solve i1 s1
      else s

answer = solve 1 0

main = print answer

{-
More imperative style.  Constant memory usage.  
This is exactly what I would write in C, except for
the use of recursion instead of a for loop.
Faster than 1a.  Still follows the problem statement closely.  
Performance is O(n).
-}
