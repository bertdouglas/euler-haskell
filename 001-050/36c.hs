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

ispal10a :: Integer -> Bool
ispal10a i = left == (reverse right)
  where
    s = show i
    n = length s
    nside = n `div` 2
    nskip = n - (2*nside) 
    left = take nside s
    right = drop (nside+nskip) s

ispal10b i = s == reverse s
  where
    s = show i

ispal10c i base = ispal10c' i base i 0
  where
    ispal10c' i base k r
      | k > 0 = ispal10c' i base (div k base) (r*base + (mod k base))
      | True  = i == r

--ispal10 = ispal10a         -- slightly slower than b
ispal10 = ispal10b           -- fastest and simplest
--ispal10 i = ispal10c i 10  -- about 50 percent slower than b

solve limit = even_sum + odd_sum
  where
    even_sum = solve' limit False 1 0
    odd_sum  = solve' limit True  1 0
    solve' :: Integer -> Bool -> Integer -> Integer -> Integer
    solve' limit odd i s
      | inrange && double = solve' limit odd (i+1) (s+p2) 
      | inrange           = solve' limit odd (i+1) s
      | True              = s
        where
          p2 = gen_pal 2 odd i
          inrange = p2 < limit
          double = ispal10 p2
          
main = do
  print $ map (gen_pal 10 True) [0..20]
  print $ map (gen_pal 10 False) [0..20]
  print $ map showBin $ map (gen_pal 2 True) [0..20]
  print $ map showBin $ map (gen_pal 2 False) [0..20]
  print $ solve (10^6)
  print $ solve (10^9)

{-
This version (36) is about 10x faster than 36b and 300x faster than 36a.

Conclude from speed testing that "show" is quite fast, and that mod and div
operations on "Integer" are surprisingly slow. 

Consider a variant of pal_gen using strings.
Consider using Int instead of Integer.  But this would limit the palindrome
size to about 10^19. Can this even be reached in practical time?  No 10^15 
does not finish in a minute.

-}

