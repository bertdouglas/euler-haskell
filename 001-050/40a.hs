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

digits :: DVU.Vector Int
digits =
  DVU.fromList
  $ take ((10^max_exp) +1)
  $ concat
  $ map dsplit 
  $ [0..]

solve =
  product
  $ map (digits DVU.!)
  $ map (10^) [0..max_exp]

main = do
  print $ DVU.slice 0 100 digits
  print solve

{-
This is a brute force method.
It stores the entire array of n=10^6 digits.
Then looks them up.
obviously this won't work for larger n.  The array is consuming
about 8 megabytes, which is trivial.  But I only have 3 gigabytes
total.  A value of n=9 would surely fail due to lack of memory.
It would also be 1000 x slower.

Next step would be to scan the digits and pick off the desired
ones as they are produced, so that it is not necessary to store 
a large array. This would reduce memory use to a small constant
amount.  But it would still be O(10^n) with time.

It would also be possible, with a little mental effort, to make
a closed form, constant time solution.
-}


