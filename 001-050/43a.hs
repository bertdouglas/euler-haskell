{-
Sub-string divisibility
Problem 43
The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

d2d3d4=406 is divisible by 2
d3d4d5=063 is divisible by 3
d4d5d6=635 is divisible by 5
d5d6d7=357 is divisible by 7
d6d7d8=572 is divisible by 11
d7d8d9=728 is divisible by 13
d8d9d10=289 is divisible by 17
Find the sum of all 0 to 9 pandigital numbers with this property.
-}

import Data.List

dsplit :: String -> [String]
dsplit ds = dsplit' (tail ds) []
  where
    dsplit' ds out
      | length ds == 3  = reverse (ds:out)
      | otherwise       = dsplit' (tail ds) ((take 3 ds):out)

-- divisibility check
div_check :: [String] -> Bool
div_check s = and [0==(i `mod` d) | (i,d) <- ids]
  where
    ints = map read s
    divs = [2,3,5,7,11,13,17]
    ids = zip ints divs

answer = sum $ map read $ filter (div_check . dsplit) $ permutations "0123456789"

main = do
  print $ dsplit "1406357289"
  print $ div_check $ dsplit "1406357289"
  print $ answer

{-
Unfortunately slow at about 35 seconds.
Have new incremental idea.  Start with just permutations of first three
then filter all that do not satisfy divisibility to that point
then extend all choices with all unused digits
then filter again, then extend again.

Also push the "and" operation down, combine the splitting and checking
so if a false is found, the rest is skipped.

It would be better to work all in ints, and get rid of the strings.
Use list of digits as ints instead of any strings/characters.  Get rid of
any show/read.

Use Int instead of Integer.
-}

