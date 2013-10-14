{-
Digit fifth powers
Problem 30
Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

1634 = 1^4 + 6^4 + 3^4 + 4^4
8208 = 8^4 + 2^4 + 0^4 + 8^4
9474 = 9^4 + 4^4 + 7^4 + 4^4
As 1 = 1^4 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
-}

import Data.Bits
import Data.List
import Data.Char

char2int :: Char -> Int
char2int c = (ord c) .&. 0xf

sump :: Int -> Int -> Int
sump p i = sum $ map ((^p) . char2int) $ show i

solve :: Int -> [Int]
solve p = solve' 2 []
  where
    solve' i out
      | done       = out
      | match      = solve' (i+1) (i:out)
      | otherwise  = solve' (i+1) out
        where 
          s = sump p i
          match = s==i
          iscale :: Int
          iscale = (10^) $ truncate $ (/) (log $ fromIntegral i) (log 10)
          done  = (s<i) && ((mod i iscale) == (iscale-1))
      
main = do
  print $ solve 4
  --print $ solve 5
  print $ sum $ solve 5



