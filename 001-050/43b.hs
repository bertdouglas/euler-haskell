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

-- check the first three digits of a list for divisibility
check :: Int -> [Int] -> Bool
check d (a:b:c:t) =  0== (mod (a*100 + b*10 + c) d)

-- filter possible choices
fcheck :: Int -> [[Int]] -> [[Int]]
fcheck d dss = filter (check d) dss

--extend partial lists of digits with 1 more possible digit
extend1 :: [[Int]] -> [[Int]]
extend1 dss = extend1' dss []
  where
    extend1' dss out
      | dss==[] = out
      | True    = extend1' (tail dss) (exts ++ out)
        where
          ds = head dss
          unused = [0..9] \\ ds
          exts = [u:ds | u <- unused]

extend :: Int -> [[Int]] -> [[Int]]
extend n dss = iterate extend1 dss !! n

collapse1 :: [Int] -> Int
collapse1 ds = foldl1 (\a b -> a*10 + b) ds
collapse dss = map collapse1 dss

solutions = 
  collapse     $
  extend 1     $
  fcheck 2     $
  extend 1     $
  fcheck 3     $
  extend 1     $
  fcheck 5     $
  extend 1     $
  fcheck 7     $
  extend 1     $
  fcheck 11    $
  extend 1     $
  fcheck 13    $
  extend 1     $
  fcheck 17    $
  extend 3 [[]]

main = do
  --print $ extend1 [[]]
  --print $ extend 1 $ extend 1 [[]]
  --print $ extend 2 [[]]
  --print $ fcheck 17 $ extend 3 [[]]
  print solutions
  print $ sum solutions

{-
This one is around 0.005 seconds.
Only problem is that it reads from the bottom up.
-}

