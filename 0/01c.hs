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

answer =
  let
    sum i =
      let
        n = (limit-1) `div` i
      in
        i * (n * (n+1)) `div` 2

    s3  = sum 3
    s5  = sum 5
    s15 = sum 15
  in
    s3 + s5 - s15

main = print answer

{-
Found a closed form solution.  Performance is O(1).
-}
