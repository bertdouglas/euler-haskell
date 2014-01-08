{-
Champernowne's constant
Problem 40
An irrational decimal fraction is created by concatenating the positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If dn represents the nth digit of the fractional part, find the value of the following expression.

d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
-}

import Data.List
import Debug.Trace

max_exp = 6

dsplit :: Int -> [Int]
dsplit n = dsplit' n []
  where
    dsplit' n s
      | n > 0     = dsplit' rest (first:s)
      | n < 0     = []
      | s == []   = [0]
      | otherwise = s
        where
          first = mod n 10
          rest  = div n 10

champ :: Int -> Int
champ n = champ' n 1
  where
    champ' n d 
      | n > m     =  champ' (n-m) (d+1)
      | otherwise =  (dsplit k) !! p
        where
          f  = 10^(d-1)           -- first number with d digits
          m  = d * f * 9          -- count of numbers with d digits
          n1 = n-1
          k  = f + (div n1 d)     -- the number containing result 
          p  = mod n1 d           -- position of digit in k

solve =
  product
  $ map champ
  $ map (10^) [0..max_exp]

main = do
  -- print $ map dfirst [1..10]
  -- print $ map dlast  [1..10]
  -- print $ map dcount [1..10]
  -- print $ map champ [1..200]
  print solve

{-
This method is O(log n) in time and O(1) in memory
-}


