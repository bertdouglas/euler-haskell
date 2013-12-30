{-
Digit factorials
Problem 34
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
-}

import Data.List
import qualified Data.Vector.Unboxed as DVU

fact_slow :: Int -> Int
fact_slow n = product [1..n]

fact_tbl :: DVU.Vector Int 
fact_tbl = DVU.fromList $ map fact_slow [0..9]

fact n = fact_tbl DVU.! n

pow10 :: [Int]
pow10 = map (10^) [0..9]

maxn = (length pf1) + 1
  where
    f = map ((fact 9)*) [1..10]
    pf = zip pow10 f
    pf1 = takeWhile (\(p,f) -> (p < f)) pf

value :: [Int] -> Int
value x = sum $ zipWith (*) pow10 x

curious :: [Int] -> Bool
curious x = (value x) == (sum $ map fact x)

non_trivial :: [Int] -> Bool
non_trivial x = (length x) > 1

step :: [Int] -> [Int]
step x = step' x []
  where
    step' x out
      | x == []    = reverse (1:out)
      | h == 9     = step' t (0:out)
      | otherwise  = (reverse ((h+1):out)) ++ t
        where 
          h = head x
          t = tail x

main = do
  print maxn
  print $ fact_tbl
  print $ map fact [0..9]
  print pow10
  print $ take 200 $ iterate step []
  print $ curious [5,4,1]
  print
    $ sum
    $ map value
    $ filter non_trivial
    $ filter curious
    $ takeWhile (\x -> (length x) < maxn)
    $ iterate step []


