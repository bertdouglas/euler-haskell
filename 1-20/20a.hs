--------------------------------------------------------------------------------
-- Factorial digit sum
-- Problem 20
-- n! means n × (n − 1) × ... × 3 × 2 × 1

-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

-- Find the sum of the digits in the number 100!
--------------------------------------------------------------------------------

import Data.Bits
import Data.Char

digit_to_integer :: Char -> Integer
digit_to_integer d = fromIntegral $ (.&.) 0xf $ ord d

fac n = fac' n 1
  where
    fac' n r
      | n==1 = r
      | n>1  = fac' (n-1) (r*n)

answer = sum $ map digit_to_integer $ show $ fac 100

main = print answer
