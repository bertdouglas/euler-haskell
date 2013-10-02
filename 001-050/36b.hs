--------------------------------------------------------------------------------
-- Double-base palindromes
-- Problem 36
-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

-- (Please note that the palindromic number, in either base, may not include leading zeros.)
--------------------------------------------------------------------------------

import Data.String.Utils
import Data.List
import Data.Char
import Data.String
import Numeric


showBin :: Integer -> String
showBin x = showIntAtBase 2 (head . show) x ""

gen_pal base odd n = gen_pal' base m n
  where
    m = if odd
      then div n base
      else n
    gen_pal' base m p
      | m > 0 = gen_pal' base (div m base) ((p * base) + (mod m base))
      | True  = p

-- get a list of palindromes up to specified limit
pal_to base odd limit = takeWhile (<limit) $ map (gen_pal base odd) [1..]

solve limit = sum u
  where
    e2  = pal_to  2 False limit
    o2  = pal_to  2 True  limit
    e10 = pal_to 10 False limit
    o10 = pal_to 10 True  limit
    p2  = o2  ++ e2
    p10 = o10 ++ e10
    u = intersect p2 p10


main = do
  print $ map (gen_pal 10 True) [0..20]
  print $ map (gen_pal 10 False) [0..20]
  print $ map showBin $ map (gen_pal 2 True) [0..20]
  print $ map showBin $ map (gen_pal 2 False) [0..20]
  print $ solve (10^6)

{-
This version (36b) is about 30x faster than 36a.  At larger numbers the
intermediate lists consume a lot of memory.  The set union cannot proceed
until all the palindromes are computed.
-}

