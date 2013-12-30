{-
Pandigital products
Problem 32
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
-}

import Data.List

digits_of :: Int -> [Int]
digits_of a = digits_of' a []
  where
    digits_of' a out
      | a > 0     = digits_of' rest (first:out)
      | otherwise = out
        where
          first = mod a 10
          rest  = div a 10

is_pan_digital :: (Int,Int,Int) -> Bool
is_pan_digital (a,b,c) = [1..9] == digits
  where
    digits = sort $ concat $ map digits_of [a,b,c]

next_id :: (Int,Int,Int) -> (Int,Int,Int)
next_id (a,b,_) 
  | ( a    * (b+1)) < 10000 = ( a,   b+1, a*(b+1)     )
  | ((a+1) * (a+2)) < 10000 = ( a+1, a+1, (a+1)*(a+2) ) 
  | otherwise               = (0,0,0)

solve = 
  sum $ nub $ sort
    $ map (\(_,_,c) -> c)
    $ filter is_pan_digital 
    $ takeWhile (/=(0,0,0))
    $ iterate next_id (2,2,0)

main = do
  print solve

