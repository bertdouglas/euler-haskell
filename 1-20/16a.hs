--------------------------------------------------------------------------------
-- Power digit sum
-- Problem 16
-- 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

-- What is the sum of the digits of the number 2^1000?
--------------------------------------------------------------------------------

import Data.Bits
import Data.Char

digit_to_integer :: Char -> Integer
digit_to_integer d = fromIntegral $ (.&.) 0xf $ ord d

answer = sum $ map digit_to_integer $ show $ 2^1000

main = print answer
