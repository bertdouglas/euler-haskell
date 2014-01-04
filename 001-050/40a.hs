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
import qualified Data.Vector.Unboxed as DVU

dsplit :: Int -> [Int]
dsplit n = dsplit' n []
  where
    dsplit' n s
      | n > 0     = dsplit' rest (first:s)
      | otherwise = s
        where
          first = mod n 10
          rest  = div n 10

digits :: DVU.Vector Int
digits =
  DVU.fromList
  $ ([0] ++)
  $ take (10^6)
  $ concat
  $ map dsplit 
  $ [1..]

solve =
  product
  $ map (digits DVU.!)
  $ map (10^) [0..6]

main = do
  print $ DVU.slice 0 100 digits
  print solve

